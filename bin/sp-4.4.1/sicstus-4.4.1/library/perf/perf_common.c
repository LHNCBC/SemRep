#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include <sys/stat.h>

#include <sicstus/sicstus.h>
#include <sicstus/config.h>

#if !defined(FORCE_BUILD_PERF)
#if !FORCE_BUILD && SICSTUS_VERSION > 40305 /* [PM] 4.3.3 postpone */
#else /* FORCE_BUILD */
#define FORCE_BUILD_PERF 1
#endif /* FORCE_BUILD */
#endif /* !defined(FORCE_BUILD_PERF) */

#define BARF(CODE) do { code = (CODE); goto barf; } while(0)
#define CHECK(CODE) do { code = (CODE); if (SPIO_FAILED(code)) { spio_t_error_code tmp_code = code; BARF(tmp_code); } } while(0)
#define NULL_CHECK(PTR) do { void const * tmp_ptr = (PTR); if (tmp_ptr == NULL) { BARF(SPIO_E_OUT_OF_MEMORY); } } while(0)

#ifndef SPTI_EMIT_PERF_ELF
/* We do not handle all ELF platforms yet. */
#if SP_POWER64 || SP_X64
#if HAVE_ELF_H
#define SPTI_EMIT_PERF_ELF 1
#endif  /* HAVE_ELF_H */
#endif /* SP_POWER64 || SP_X64 */
#else
#error "SPTI_EMIT_PERF_ELF is defined"
#endif  /* SPTI_EMIT_PERF_ELF */

#if HAVE_ELF_H
#include <elf.h>
#endif  /* HAVE_ELF_H */

#if SP_SIZEOF_VOID_P == 4 /* 32-bit. */

#define ELFCLASSXX ELFCLASS32

#define ELFXX_ST_INFO ELF32_ST_INFO
#define ELFXX_ST_VISIBILITY ELF32_ST_VISIBILITY
#define ElfXX_Addr Elf32_Addr
#define ElfXX_ST_NAME Elf32_Word /* Elf32_Syminfo.st_name */
#define ElfXX_ST_SIZE Elf32_Word /* Elf32_Syminfo.st_size */
#define ElfXX_Half Elf32_Half

#define ElfXX_Sym Elf32_Sym
#define ElfXX_Shdr Elf32_Shdr
#define ElfXX_Phdr Elf32_Phdr
#define ElfXX_Ehdr Elf32_Ehdr

#elif SP_SIZEOF_VOID_P == 8  /* 64-bit */

#define ELFCLASSXX ELFCLASS64

#define ELFXX_ST_INFO ELF64_ST_INFO
#define ELFXX_ST_VISIBILITY ELF64_ST_VISIBILITY
#define ElfXX_Addr Elf64_Addr
#define ElfXX_ST_NAME Elf64_Word /* Elf64_Syminfo.st_name */
#define ElfXX_ST_SIZE Elf64_Xword /* Elf64_Syminfo.st_size */
#define ElfXX_Half Elf64_Half

#define ElfXX_Sym Elf64_Sym
#define ElfXX_Shdr Elf64_Shdr
#define ElfXX_Phdr Elf64_Phdr
#define ElfXX_Ehdr Elf64_Ehdr

#endif /* 64-bit */


typedef struct spti_t_cookie_ spti_t_cookie;
struct spti_t_cookie_ {
  int inited;
};

static char *stralloc(char const *strings[])
{
  spio_t_error_code code = SPIO_E_INTERNAL_ERROR;
  char *s = NULL;
  char const *tmp;
  size_t len;
  int i;

  len = 0;
  i = 0;
  while((tmp = strings[i++]) != NULL)
      {
        size_t tmp_len = strlen(tmp);
        len += tmp_len;
      }

  s = malloc(len +1);        /* +1 for NUL */
  NULL_CHECK(s);

  len = 0;
  i = 0;
  while((tmp = strings[i++]) != NULL)
    {
      size_t tmp_len = strlen(tmp);
      memcpy(s+len, tmp, tmp_len);
      len += tmp_len;
    }
  s[len] = '\0';
 cleanup:
  return s;
 barf:
  (void)code;
  if (s != NULL)
    {
      free(s);
      s = NULL;
    }
  goto cleanup;
}

typedef struct buf_ perf_t_buf;
struct buf_ {
  size_t size;
  void *data;
#define PERF_BUF_FLAG_FREE_BUF 0x0001 /* Deallocate should do free(buf). */
#define PERF_BUF_FLAG_FREE_DATA 0x0002 /* Deallocate should do free(buf->data). */
  spio_t_bits flags;
  perf_t_buf *next;
};

/*
  Free the entire chain of bufs and their data (subject to flags).
  buf: may be null
*/
static void free_buf(perf_t_buf *buf_)
{
  perf_t_buf *next = buf_;
  while (next != NULL) {
    perf_t_buf *current = next;
    next = current->next;
    if (SPIO_MASK_IS_SET(current->flags, PERF_BUF_FLAG_FREE_DATA)) {
      if (current->data != NULL) {
        free(current->data);
        current->data = NULL;
      }
    }
    if (SPIO_MASK_IS_SET(current->flags, PERF_BUF_FLAG_FREE_BUF)) {
      current->data = NULL;
      free(current);
    }
  }
}

/*
  Allocate a buffer with the specifed data. Returns NULL on error.
  data: may be null.

  Note: the data will not automatically be freed unless you set PERF_BUF_FLAG_FREE_DATA.
*/
static perf_t_buf *alloc_buf_data(size_t size, void const *data)
{
  perf_t_buf *buf = malloc(sizeof *buf);
  if (buf != NULL) {
    buf->size = size;
    buf->data = (void*) data;
    buf->flags = SPIO_OPTION_NONE;
    SPIO_SET_MASK(buf->flags, PERF_BUF_FLAG_FREE_BUF);
    buf->next = NULL;
  }
  return buf;
}

/*
  Allocate a buffer with size bytes of data. Returns NULL on error.
  size: number of bytes in buf->data.
  clear: if true, use calloc instead of malloc.
 */
static perf_t_buf *alloc_buf_size_(size_t size, int clear)
{
  perf_t_buf *buf;
  size_t alloc_size = (sizeof *buf) + size;

  buf = (clear ? calloc(alloc_size, 1) : malloc(alloc_size));

  if (buf != NULL) {
    buf->size = size;
    buf->data = ((char *)buf)+(sizeof *buf);
    buf->flags = SPIO_OPTION_NONE;
    SPIO_SET_MASK(buf->flags, PERF_BUF_FLAG_FREE_BUF);
    /* No, freed as part of buf itself: SPIO_SET_MASK(buf->flags, PERF_BUF_FLAG_FREE_DATA); */
    buf->next = NULL;
  }
  return buf;
}

static perf_t_buf *alloc_buf_size(size_t size)
{
  return alloc_buf_size_(size, 0);
}

static perf_t_buf *calloc_buf_size(size_t size)
{
  return alloc_buf_size_(size, 1);
}


/* Immutable. The string table for section names (".shstrtab") */
static char const shstrtab_template[] = {
  /* "The first byte, which is index zero, holds a null character." */
#define SH_STR_NUL 0
  '\0',
#define SH_STR_TEXT (SH_STR_NUL + 1)
  '.', 't', 'e', 'x', 't', '\0',
#define SH_STR_SHSTRTAB (SH_STR_TEXT + 6)
  '.', 's', 'h', 's', 't', 'r', 't', 'a', 'b', '\0',
#define SH_STR_SYMTAB   (SH_STR_SHSTRTAB + 10)
  '.', 's', 'y', 'm', 't', 'a', 'b', '\0',
#define SH_STR_STRTAB   (SH_STR_SYMTAB + 8)
  '.', 's', 't', 'r', 't', 'a', 'b', '\0',
};

/* Our section numbers */
enum section_header_number {
  /* in elf.h: SHN_UNDEF = 0, */
  SHN_TEXT=1,   /* ".text" is section #1. */
  SHN_SHSTRTAB=2, /* ".shstrtab" is section #2 */
  SHN_SYMTAB=3,
  SHN_STRTAB=4,
  N_SHDRS=5
};
/* Currently only have one entry, the global entry for symbol_name. */
#define SYMTAB_FIRST_GLOBAL_ENTRY 0

static spio_t_error_code perf_create_elf(char const *symbol_name, void const *addr, size_t size, perf_t_buf **pbuf)
{
  spio_t_error_code code = SPIO_E_INTERNAL_ERROR;
  perf_t_buf *buf = NULL;

  size_t const ehdr_size = sizeof(ElfXX_Ehdr);
  perf_t_buf *ehdr_buf = calloc_buf_size(ehdr_size); /* calloc_... ensure clear e_ident[] padding */
  size_t const phdr_size = 1 * sizeof(ElfXX_Phdr);
  perf_t_buf *phdr_buf = alloc_buf_size(phdr_size);
  size_t const text_segment_size = size;
  perf_t_buf *text_buf = alloc_buf_data(text_segment_size, addr);
  size_t const shstrtab_size = sizeof shstrtab_template;
  perf_t_buf *shstrtab_buf = alloc_buf_data(shstrtab_size, shstrtab_template);
  size_t const symtab_size = 1 * sizeof(ElfXX_Sym);
  perf_t_buf *symtab_buf = alloc_buf_size(symtab_size);
  /* Only the symbol name goes here. There does not seem to be a need
     for a STT_FILE entry or a NUL entry in .strtab */
  size_t const strtab_size = strlen(symbol_name) +1; /* +1 includes '\0' */
  perf_t_buf *strtab_buf = alloc_buf_data(strtab_size, symbol_name);
  size_t const shdr_size = N_SHDRS * sizeof(ElfXX_Shdr);
  perf_t_buf *shdr_buf = alloc_buf_size(shdr_size);

  if (SPIO_FALSE
      || ehdr_buf == NULL
      || phdr_buf == NULL
      || text_buf == NULL
      || shstrtab_buf == NULL
      || symtab_buf == NULL
      || strtab_buf == NULL
      || shdr_buf == NULL) {
    BARF(SPIO_E_OUT_OF_MEMORY);
  }
  
  {
    ElfXX_Ehdr * ehdr = (ElfXX_Ehdr *) ehdr_buf->data;
    ElfXX_Phdr * phdr = (ElfXX_Phdr *) phdr_buf->data;
    /* char const * shstrtab = (char const*) shstrtab_buf->data; */
    ElfXX_Sym * symtab = (ElfXX_Sym *) symtab_buf->data;
    /* char * strtab = (char *) strtab_buf->data; */
    ElfXX_Shdr * shdr = (ElfXX_Shdr *) shdr_buf->data;
    ElfXX_ST_NAME strtab_index_symbol_name;

    /* size_t ehdr_offset; */
    size_t phdr_offset;
    size_t text_segment_offset;
    size_t shstrtab_offset;
    size_t symtab_offset;
    size_t strtab_offset;
    size_t shdr_offset;


    /* Section ".strtab" */
    {
      /* strtab[] already contains its single entry, the symbol_name. */
      
      strtab_index_symbol_name = 0; /* symbol_name starts at &strtab[strtab_index_symbol_name]. */
    }
       
    /* Section ".symtab" */
    {
      symtab[0].st_name = strtab_index_symbol_name; /* Symbol name, index in string tbl, i.e. &strtab[st_name]. */
      /* FIXME: Would it make more sense to use STB_LOCAL, in case we redefined the underlying predicate? */
      symtab[0].st_info = ELFXX_ST_INFO(STB_GLOBAL, STT_FUNC);
      symtab[0].st_other = ELFXX_ST_VISIBILITY(STV_DEFAULT);
      symtab[0].st_shndx = SHN_TEXT; /* section index (we use 1 for the ".text" segment).*/
      symtab[0].st_value = (ElfXX_Addr) addr;
      symtab[0].st_size = (ElfXX_ST_SIZE) size;
    }

    /* Sections and their offset
       
       [ "Elf Header | "Program Header" | <HERE> ".text" | ".shstrtab" | ".symtab" | ".strtab" | Section Header ]
    */
    {
      size_t file_offset = 0;

      /* ehdr_offset = 0; */
      file_offset += ehdr_size;

      phdr_offset = file_offset;
      file_offset += phdr_size;

      text_segment_offset = file_offset;
      file_offset += text_segment_size;

      shstrtab_offset = file_offset;
      file_offset += shstrtab_size;

      symtab_offset = file_offset;
      file_offset += symtab_size;
    
      strtab_offset = file_offset;
      file_offset += strtab_size;

      shdr_offset = file_offset;
      file_offset += shdr_size;
      (void) file_offset;
    }



    /* ELF Header */
    {
      /* e_ident */
      {
        /* memset(ehdr->e_ident, 0, EI_NIDENT); */
        ehdr->e_ident[EI_MAG0] = ELFMAG0;
        ehdr->e_ident[EI_MAG1] = ELFMAG1;
        ehdr->e_ident[EI_MAG2] = ELFMAG2;
        ehdr->e_ident[EI_MAG3] = ELFMAG3;
        ehdr->e_ident[EI_CLASS] = ELFCLASSXX;
        ehdr->e_ident[EI_DATA] = ELFDATA2LSB;
        ehdr->e_ident[EI_VERSION] = EV_CURRENT;
        /* a.k.a. ELFOSABI_SYSV. Note: ELFOSABI_LINUX is not what is
           commonly used on Linux, it seems. */
        ehdr->e_ident[EI_OSABI] = ELFOSABI_NONE;

        /* "If no values are specified for the EI_OSABI field by the
           processor supplement or no version values are specified for
           the ABI determined by a particular value of the EI_OSABI
           byte, the value 0 shall be used for the EI_ABIVERSION byte;
           it indicates unspecified."
        */
        ehdr->e_ident[EI_ABIVERSION] = 0;
      }

      ehdr->e_type = ET_EXEC;                    /* Object file type "Executable file" */

#if SP_X64
      ehdr->e_machine = EM_X86_64; /* 32-bit would be EM_386 */
      ehdr->e_version = EV_CURRENT;
      ehdr->e_flags = 0;         /* Processor-specific flags */
#elif SP_X32
      ehdr->e_machine = EM_386;
      ehdr->e_version = EV_CURRENT;
      ehdr->e_flags = 0;         /* Processor-specific flags */
#elif SP_POWER64
      ehdr->e_machine = EM_PPC64;
      ehdr->e_version = EV_CURRENT;
      /*
        "E_flags defining the ABI level:
        0 For ELF object files of an unspecified nature.

        1 For the Power ELF V1 ABI using function descriptors. This
        ABI is currently only used for big-endian PowerPC
        implementations.

        2 For the OpenPOWER ELF V2 ABI using the facilities described
        here and including function pointers to directly reference
        functions.
        "
      */
      ehdr->e_flags = 2;
#else
      ehdr->e_machine = EM_NONE;
#endif

      /* 
         FIXME: Would zero suffice?

         "The virtual address to which the system first transfers
         control, thus starting the process. If the file has no
         associated entry point, this member holds zero."
      */
      ehdr->e_entry = (ElfXX_Addr)addr;

      /*
        "The program header table's file offset in bytes. If the file
        has no program header table, this member holds zero."
      */
      ehdr->e_phoff = phdr_offset;
      /*
        "The section header table's file offset in bytes. If the file
        has no section header table, this member holds zero."
      */
      ehdr->e_shoff = shdr_offset;

      /* "The ELF header's size in bytes." */
      ehdr->e_ehsize = sizeof(ElfXX_Ehdr);

      /* "The size in bytes of one entry in the file's program header table. All entries are the same size." */
      ehdr->e_phentsize = sizeof(ElfXX_Phdr);
      /*
        "The number of entries in the program header table. The
        product of e_phentsize and e_phnum gives the table's size in
        bytes. If a file has no program header table, e_phnum holds
        the value zero."
      */
      ehdr->e_phnum = 1;
      /*
        "A section header's size in bytes. A section header is one
        entry in the section header table. All entries are the same
        size."
      */
      ehdr->e_shentsize = sizeof(ElfXX_Shdr);
      /*
        "The number of entries in the section header table. The
        product of e_shentsize and e_shnum gives the section header
        table's size in bytes. If a file has no section header table,
        e_shnum holds the value zero."
      */
      ehdr->e_shnum = N_SHDRS;
      /*
        "The section header table index of the entry that is
        associated with the section name string table. If the file has
        no section name string table, this member holds the value
        SHN_UNDEF"
      */
      ehdr->e_shstrndx = SHN_SHSTRTAB;
    }

    /* Program Header Table */
    {
      /*
        "The kind of segment this array element describes or how to
        interpret the array element's information."

        PT_LOAD: "Specifies a loadable segment, described by p_filesz
        and p_memsz. The bytes from the file are mapped to the
        beginning of the memory segment. If the segment's memory size
        (p_memsz) is larger than the file size (p_filesz), the extra
        bytes are defined to hold the value 0. These bytes follow the
        initialized area of the segment. The file size can not be
        larger than the memory size. Loadable segment entries in the
        program header table appear in ascending order, and are sorted
        on the p_vaddr member."
      */
      phdr->p_type = PT_LOAD;
      /* "The offset from the beginning of the file at which the first byte of the segment resides." */
      phdr->p_offset = phdr_offset;
      /* "The virtual address at which the first byte of the segment resides in memory." */
      phdr->p_vaddr = (ElfXX_Addr)addr;
      /* "The segment's physical address for systems in which physical
         addressing is relevant. Because the system ignores physical
         addressing for application programs, this member has
         unspecified contents for executable files and shared
         objects." */
      phdr->p_paddr = (ElfXX_Addr)addr;
      /* "The number of bytes in the file image of the segment, which can be zero." */
      phdr->p_filesz = text_segment_size;
      /* "The number of bytes in the memory image of the segment, which can be zero." */
      phdr->p_memsz = text_segment_size;
      /* "Flags that are relevant to the segment." 

         "ELF Segment Permissions: PF_R + PF_X Read, execute"
      */
      phdr->p_flags = PF_X|PF_R;
      /* "Loadable process segments must have congruent values for
         p_vaddr and p_offset, modulo the page size. This member gives
         the value to which the segments are aligned in memory and in
         the file. Values 0 and 1 mean no alignment is
         required. Otherwise, p_align should be a positive, integral
         power of 2, and p_vaddr should equal p_offset, modulo
         p_align." */

      /* [PM] I think we can promise larger alignment, but I do not think it matters. */
      phdr->p_align = 4;
    }


      


    /* Section Headers (goes last in the file) */
    {
      /* shdr[SHN_UNDEF] */
      {
        /* "... the section header for index 0 (SHN_UNDEF) exists,
           even though the index marks undefined section
           references. This entry holds the following. "
        */
        shdr[SHN_UNDEF].sh_name = 0; /* "No name" */
        shdr[SHN_UNDEF].sh_type = SHT_NULL;
        shdr[SHN_UNDEF].sh_flags = 0;
        shdr[SHN_UNDEF].sh_addr = 0;
        shdr[SHN_UNDEF].sh_offset = 0;
        shdr[SHN_UNDEF].sh_size = 0; /* "Unspecified. If non-zero, the actual number of section header entries." */
        shdr[SHN_UNDEF].sh_link = 0; /* "Unspecified. If non-zero, the index of the section header string table section." */
        shdr[SHN_UNDEF].sh_info = 0;
        shdr[SHN_UNDEF].sh_addralign = 0;
        shdr[SHN_UNDEF].sh_entsize = 0;
      }

      /* shdr[SHN_TEXT] */
      {
        /*
          http://refspecs.linuxbase.org/LSB_3.0.0/LSB-PDA/LSB-PDA/specialsections.html
          ".text        SHT_PROGBITS    SHF_ALLOC+SHF_EXECINSTR"
        */
        shdr[SHN_TEXT].sh_name = SH_STR_TEXT; /* ".text" ("index into the section header string table section") */
        shdr[SHN_TEXT].sh_type = SHT_PROGBITS;
        shdr[SHN_TEXT].sh_flags = SHF_ALLOC | SHF_EXECINSTR;
        shdr[SHN_TEXT].sh_addr = (ElfXX_Addr)addr;
        shdr[SHN_TEXT].sh_offset = text_segment_offset;
        shdr[SHN_TEXT].sh_size = text_segment_size;
        shdr[SHN_TEXT].sh_link = 0;
        shdr[SHN_TEXT].sh_info = 0;
        shdr[SHN_TEXT].sh_addralign = 4;        /* JITed code is nominally word aligned */

        /* "Some sections hold a table of fixed-size entries, such as
           a symbol table. For such a section, this member gives the
           size in bytes of each entry. The member contains the value
           zero if the section does not hold a table of fixed-size
           entries." */
        shdr[SHN_TEXT].sh_entsize = sizeof(ElfXX_Shdr);
      }
      
      /* shdr[SHN_SHSTRTAB] */
      {
        shdr[SHN_SHSTRTAB].sh_name = SH_STR_SHSTRTAB; /* ".shstrtab" "Section names." */
        shdr[SHN_SHSTRTAB].sh_type = SHT_STRTAB;
        /* "SHF_STRINGS: Identifies a section that consists of
           null-terminated character strings. The size of each
           character is specified in the section header's sh_entsize
           field." */
        shdr[SHN_SHSTRTAB].sh_flags = SHF_STRINGS;
        /* "sh_addr: If the section appears in the memory image of a
           process, this member gives the address at which the
           section's first byte should reside. Otherwise, the member
           contains the value zero." */
        shdr[SHN_SHSTRTAB].sh_addr = 0;
        shdr[SHN_SHSTRTAB].sh_offset = shstrtab_offset;
        shdr[SHN_SHSTRTAB].sh_size = shstrtab_size;
        shdr[SHN_SHSTRTAB].sh_link = 0;
        shdr[SHN_SHSTRTAB].sh_info = 0;
        /* "Values 0 and 1 mean the section has no alignment constraints." */
        shdr[SHN_SHSTRTAB].sh_addralign = 0;
        /* "The member contains the value zero if the section does not
           hold a table of fixed-size entries."
           
           "SHF_STRINGS: ... The size of each character is specified
           in the section header's sh_entsize field."
        */
        shdr[SHN_SHSTRTAB].sh_entsize = 1;
      }

      /* shdr[SHN_SYMTAB] */
      {
        shdr[SHN_SYMTAB].sh_name = SH_STR_SYMTAB; /* ".symtab" */

        shdr[SHN_SYMTAB].sh_type = SHT_SYMTAB;
        /* "SHF_INFO_LINK: This section headers sh_info field holds a
           section header table index."

FIXME: Why does mkelf.c use SHF_STRINGS here? It makes no sense

           "SHF_STRINGS: Identifies a section that consists of
           null-terminated character strings. The size of each
           character is specified in the section header's sh_entsize
           field."
         */
        shdr[SHN_SYMTAB].sh_flags = /* SHF_STRINGS | */ SHF_INFO_LINK;
        shdr[SHN_SYMTAB].sh_addr = 0;
        shdr[SHN_SYMTAB].sh_offset = symtab_offset;
        shdr[SHN_SYMTAB].sh_size = symtab_size;
        /* SHT_SYMTAB: "sh_link: The section header index of the
           associated string table." */
        shdr[SHN_SYMTAB].sh_link = SHN_STRTAB;
        /* SHT_SYMTAB: "sh_info: One greater than the symbol table
           index of the last local symbol (binding STB_LOCAL)." */
        shdr[SHN_SYMTAB].sh_info = SYMTAB_FIRST_GLOBAL_ENTRY;
        shdr[SHN_SYMTAB].sh_addralign = sizeof(long);
        shdr[SHN_SYMTAB].sh_entsize = sizeof(ElfXX_Sym);
      }

      /* shdr[SHN_STRTAB] */
      {
        /* ".strtab" "Strings, most commonly the strings that
           represent the names associated with symbol table
           entries. " */
        shdr[SHN_STRTAB].sh_name = SH_STR_STRTAB; /* ".strtab" */
        shdr[SHN_STRTAB].sh_type = SHT_STRTAB;
        shdr[SHN_STRTAB].sh_flags = SHF_STRINGS;
        shdr[SHN_STRTAB].sh_addr = 0;
        shdr[SHN_STRTAB].sh_offset = strtab_offset;
        shdr[SHN_STRTAB].sh_size = strtab_size;
        shdr[SHN_STRTAB].sh_link = 0;
        shdr[SHN_STRTAB].sh_info = 0;
        shdr[SHN_STRTAB].sh_addralign = 1;
        shdr[SHN_STRTAB].sh_entsize = 1;
      }
    }

    /* link all buffers into buf (reverse order), passing ownership to buf. */
    strtab_buf->next = shdr_buf; shdr_buf = NULL;
    symtab_buf->next = strtab_buf; strtab_buf = NULL;
    shstrtab_buf->next = symtab_buf; symtab_buf = NULL;
    text_buf->next = shstrtab_buf; shstrtab_buf = NULL;
    phdr_buf->next = text_buf; text_buf = NULL;
    ehdr_buf->next = phdr_buf; phdr_buf = NULL;
    buf = ehdr_buf; ehdr_buf = NULL;
    
    /*
      write(fh, &ehdr, sizeof(Elf64_Ehdr));
      write(fh, &phdr, sizeof(Elf64_Phdr));
      write(fh, text, text_sz);
      write(fh, shstrtab, SHSTRTAB_SZ);
      write(fh, symtab, sizeof(symtab));
      write(fh, strtab, strtab_sz);
      write(fh, shdr, N_SHDRS * sizeof(Elf64_Shdr));
    */
  }

  if (pbuf != NULL) {
    *pbuf = buf;
    buf = NULL; /* protect from cleanup */
  }
  code = SPIO_S_NOERR;
 cleanup:
  if (buf != NULL) {
    free_buf(buf);
    buf = NULL;
  }
  if (ehdr_buf != NULL) {
    free_buf(ehdr_buf);
    ehdr_buf = NULL;
  }
  if (phdr_buf != NULL) {
    free_buf(phdr_buf);
    phdr_buf = NULL;
  }
  if (text_buf != NULL) {
    free_buf(text_buf);
    text_buf = NULL;
  }
  if (shstrtab_buf != NULL) {
    free_buf(shstrtab_buf);
    shstrtab_buf = NULL;
  }
  if (symtab_buf != NULL) {
    free_buf(symtab_buf);
    symtab_buf = NULL;
  }
  if (strtab_buf != NULL) {
    free_buf(strtab_buf);
    strtab_buf = NULL;
  }
  if (shdr_buf != NULL) {
    free_buf(shdr_buf);
    shdr_buf = NULL;
  }

  return code;
 barf:
  goto cleanup;
}

static spio_t_uint32 unique_name_counter = 0;

/* Create the directory (but not its ancestors) if necessary.

   Succeeds with SPIO_S_OPERATION_FAILED if dir already exists (even
   if it not a directory). This makes it possible for the caller to
   determine whether the directory was created by the call.
 */
static spio_t_error_code ensdir(char const *dir, mode_t mode)
{
  spio_t_error_code code = SPIO_E_INTERNAL_ERROR;

  if (mkdir(dir, mode) != 0) {
    int e = errno;
    switch (e) {
    case EEXIST:
      /* hopefully an existing directory. */
      code = SPIO_S_OPERATION_FAILED;
      break;
    default:
      BARF(SPIO_E_OS_ERROR);
    }
  } else {
    code = SPIO_S_NOERR;
  }

 cleanup:
  return code;
 barf:
  goto cleanup;
}

static spio_t_error_code perf_write_native_code(SPAPI_ARG_PROTO_DECL
						char const *symbol_name, void const *addr, size_t size,
                                                char const *dir)
{
  spio_t_error_code code = SPIO_E_INTERNAL_ERROR;
  SP_stream *map_stream = NULL;
  SP_stream *stream = NULL;
  perf_t_buf *buf = NULL;
  int retries = 0;
  char *map_path = NULL;
  char *symlink_path = NULL;
  char *path = NULL;
  char const *components[7];
  char start_addr_as_string[(2*8) +1]; /* room for 64 bits (8 bytes, 16 nibbles) as hexadecimal. +1 for NUL. */
  char end_addr_as_string[(2*8) +1]; /* room for 64 bits (8 bytes, 16 nibbles) as hexadecimal. +1 for NUL. */
  char size_as_string[(2*8) +1]; /* room for 64 bits (8 bytes, 16 nibbles) as hexadecimal. +1 for NUL. */

  char pid_as_string[20 +1]; /* room for 64 bits as decimal ((2**64)-1 == "18446744073709551615", 20 digits). +1 for NUL. */
  char name_suffix[10 +1]; /* room for 32-bits as decimal ((2**32)-1 == 4294967295, 10 digits). +1 for NUL. */
  char const *suffix = ".elf";
  pid_t pid = getpid();
  mode_t mode = S_IRWXU;
  name_suffix[0] = '\0';

  (void) snprintf(start_addr_as_string, sizeof start_addr_as_string, "%lx", (unsigned long) addr);
  (void) snprintf(end_addr_as_string, sizeof end_addr_as_string, "%lx", (unsigned long) (((char*)addr)+size));
  (void) snprintf(size_as_string, sizeof size_as_string, "%lx", (unsigned long) size);

  CHECK(ensdir(dir, mode));

  snprintf(pid_as_string, sizeof pid_as_string, "%lu", (unsigned long) pid);

#if !FORCE_BUILD_PERF && SICSTUS_RELEASE_BUILD && SICSTUS_VERSION > 40401 /* [PM] 4.3.4 postpone */
#error "Clean up file/dir creation (do it once, at init time), etc."
#endif

  {
    int i = 0;
    size_t dir_len = strlen(dir);
    components[i++] = dir; /* Should be "/"-terminated */
    if (dir_len > 0 && dir[dir_len-1] != '/') {
      components[i++] = "/"; /* but, better safe than sorry. */
    }
    components[i++] = "perf.map";
    components[i++] = NULL;
    SP_ASSERT(i <= (sizeof components)/(sizeof components[0]));
  }
  NULL_CHECK(map_path = stralloc(components));

  /* Ensure there is a symlink /tmp/perf-PID.map -> map_path that perf
     report can find.

     FIXME: this too should only be done once.
   */
  {
    {
      int i = 0;
      size_t dir_len = strlen(dir);
      components[i++] = dir; /* Should be "/"-terminated */
      if (dir_len > 0 && dir[dir_len-1] != '/') {
	components[i++] = "/"; /* but, better safe than sorry. */
      }
      components[i++] = "../perf-"; /* move it up one level, e.g. from /tmp/sicstus_jit_PID/ to /tmp/ */
      components[i++] = pid_as_string;
      components[i++] = ".map";
      components[i++] = NULL;
      SP_ASSERT(i <=  (sizeof components)/(sizeof components[0]));
    }
    NULL_CHECK(symlink_path = stralloc(components));
    if (symlink(map_path, symlink_path) != 0) {
      int e = errno;
      switch (e) {
      case EEXIST:
	/* expected case, we have already created the symbilic link. */
	break;
      default:
	/* Some other error. Just ignore it, the symlink is not that important. */
	break;
      }
    }
  }

  CHECK(SP_fopen(map_path, NULL, (SP_FOPEN_OPTION_WRITE|SP_FOPEN_OPTION_BINARY|SP_FOPEN_OPTION_NO_CASE_NORMALIZATION|SP_FOPEN_OPTION_NOEXPAND|SP_FOPEN_OPTION_APPEND), &map_stream));

  /* FIXME: This is horrific.
     Even if we want to write on-the-fly, we should leave the stream open (and let autoflush move it to disk?).
  */
  {
    CHECK(SP_put_bytes(map_stream, (spio_t_uint8*) start_addr_as_string, strlen(start_addr_as_string), SPIO_OPTION_NONE));
    CHECK(SP_put_bytes(map_stream, (spio_t_uint8*) " ", 1, SPIO_OPTION_NONE));

    CHECK(SP_put_bytes(map_stream, (spio_t_uint8*) size_as_string, strlen(size_as_string), SPIO_OPTION_NONE));
    CHECK(SP_put_bytes(map_stream, (spio_t_uint8*) " ", 1, SPIO_OPTION_NONE));

    CHECK(SP_put_bytes(map_stream, (spio_t_uint8*) symbol_name, strlen(symbol_name), SPIO_OPTION_NONE));
    CHECK(SP_put_bytes(map_stream, (spio_t_uint8*) "\n", 1, SPIO_OPTION_NONE));
    CHECK(SP_fclose(map_stream, SPIO_OPTION_NONE));
    map_stream = NULL;
  }


  {
    int i = 0;
    components[i++] = dir;
    if (dir[strlen(dir)] != '/') {
      components[i++] = "/";
    }
    /* By using the address as part of the name we make it very likely
       that the name is unique. The directory should be process-unique
       so the only way an existing file could happen would be if we
       re-use the same address (and size!) for some new code. */
#if 1
    components[i++] = start_addr_as_string;
    components[i++] = "..";
    components[i++] = end_addr_as_string;
#else
    components[i++] = base;
    components[i++] = name_suffix;
#endif
    components[i++] = suffix;
    components[i++] = NULL;
    SP_ASSERT(i <= (sizeof components)/(sizeof components[0]));
  }

  CHECK(perf_create_elf(symbol_name, addr, size, &buf));

 retry:  
  NULL_CHECK(path = stralloc(components));

  code = SP_fopen(path, NULL, (SP_FOPEN_OPTION_WRITE|SP_FOPEN_OPTION_BINARY|SP_FOPEN_OPTION_NO_CASE_NORMALIZATION|SP_FOPEN_OPTION_NOEXPAND|SP_FOPEN_OPTION_NEW), &stream);
  if (code == SPIO_E_FILE_EXISTS && retries++ < 100) {
#if DBG && 0
    fprintf(stderr, "Retrying #%d for %s%s\n", (int)retries, dir, base); fflush(stderr);
#endif
    snprintf(name_suffix, sizeof name_suffix, "%u", (unsigned) unique_name_counter++);
    free(path);
    path = NULL;
    goto retry;
  }
  CHECK(code);
  {
    perf_t_buf *tmp = buf;
    while (tmp != NULL) {
      CHECK(SP_put_bytes(stream, tmp->data, tmp->size, SPIO_OPTION_NONE));
      tmp = tmp->next;
    }
  }
  CHECK(SP_fclose(stream, SPIO_OPTION_NONE));
  stream = NULL; /* protect from cleanup */

  
  code = SPIO_S_NOERR;
 cleanup:
  if (map_path != NULL) {
    free(map_path);
    map_path = NULL;
  }
  if (symlink_path != NULL) {
    free(symlink_path);
    symlink_path = NULL;
  }

  if (path != NULL) {
    free(path);
    path = NULL;
  }
  if (buf != NULL) {
    free_buf(buf);
    buf = NULL;
  }
  if (map_stream != NULL) {
    (void) SP_fclose(map_stream, SP_FCLOSE_OPTION_FORCE);
    map_stream = NULL;
  }

  if (stream != NULL) {
    (void) SP_fclose(stream, SP_FCLOSE_OPTION_FORCE);
    stream = NULL;
  }

  return code;
 barf:
  goto cleanup;
}


#define JIT_DEFINED_OPTION_DEFINED   0x00001
#define JIT_DEFINED_OPTION_UNDEFINED 0x00002
static void jit_defined(SPAPI_ARG_PROTO_DECL
			char const* name, SP_integer arity, char const *module, void const *addr, size_t size, spti_t_cookie *cookie, spio_t_bits options)
{
  spio_t_error_code code = SPIO_E_INTERNAL_ERROR;
  int const defined = SPIO_MASK_IS_SET(options, JIT_DEFINED_OPTION_DEFINED);
  char *dir = NULL; /* needs cleanup */
  char *symbol_name = NULL; /* needs cleanup */
  char *dir_template = NULL; /* needs cleanup */
  (void) cookie;

  if (defined) {
    char arity_name[4]; /* 255\0 */
    char pid_as_string[20 +1]; /* room for 64 bits as decimal ((2**64)-1 == "18446744073709551615", 20 digits). +1 for NUL. */
    char const *components[6];
    pid_t pid = getpid();

          
    snprintf(arity_name, sizeof arity_name, "%u", (int)(arity&0xFF));
    snprintf(pid_as_string, sizeof pid_as_string, "%lu", (unsigned long) pid);

    {
      int i = 0;
      components[i++]=module;
      components[i++] = ":";
      components[i++] = name;
      components[i++] = "/";
      components[i++] = arity_name;
      components[i++] = NULL;
      SP_ASSERT(i <=  (sizeof components)/(sizeof components[0]));
    }
    NULL_CHECK(symbol_name = stralloc(components));
    
    /* xref sp_jit_ojdump script */
    {
      int i = 0;
      components[i++]="$SP_TEMP_DIR/sicstus_jit_";
      components[i++] = pid_as_string;
      components[i++] = "/";
      components[i++] = NULL;
      SP_ASSERT(i <=  (sizeof components)/(sizeof components[0]));
    }
    NULL_CHECK(dir_template = stralloc(components));

    CHECK(SP_expand_file_name(dir_template, NULL, SP_EXPAND_FILE_NAME_OPTION_DIR, &dir));

    CHECK(perf_write_native_code(SPAPI_ARG
				 symbol_name, addr, size, dir));
  } else {
    /* FIXME: We have no way to tell perf that addr is about to be
       re-used for other purposes (including, possibly, code for
       some other procedure) */
    /* perf_undefine_native_code(symbol_name, addr, size); */
  }

  code = SPIO_S_NOERR;
 cleanup:
  if (symbol_name != NULL) {
    free(symbol_name);
    symbol_name = NULL;
  }
  if (dir_template != NULL) {
    free(dir_template);
    dir_template = NULL;
  }
  if (dir != NULL) {
    SP_free(dir); /* this is allocated by SP_expand_file_name(), using SP_malloc(). */
    dir = NULL;
  }
  (void)code;
  return;
 barf:
  goto cleanup;
}

static spio_t_error_code SPCDECL spti_hook_dummy(sp_t_spti_event *event, void *cookie_)
{
  (void)event; (void)cookie_;
  return SPIO_S_NOERR;
}

static spio_t_error_code SPCDECL spti_hook(sp_t_spti_event *event, void *cookie_)
{
  spio_t_error_code code = SPIO_E_INTERNAL_ERROR;
  spti_t_cookie *cookie = (spti_t_cookie *)cookie_;
  SPAPI_ARG_LOCAL_DECL
  SPIO_REFERENCE_BARF_LABEL();
  (void)event;
#if MULTI_SP_AWARE
  SPAPI_ARG_NAME = event->spenv;
#endif	/* MULTI_SP_AWARE */

  if (event->size != sizeof *event)
    {
      BARF(SPIO_E_PARAMETER_ERROR);
    }
  if (! (event->flags & SPTI_EVENT_FLAG_INITED))
    {
      BARF(SPIO_E_INVALID_STATE);
    }
  if (cookie == NULL)
    {
      BARF(SPIO_E_INVALID_STATE);
    }

  switch (event->request)
    {
    case SPTI_REQUEST_JIT_START:
    case SPTI_REQUEST_JIT_END:
      /* Not used by PERF SPTI */
      break;

    case SPTI_REQUEST_JIT_DEFINED:
    case SPTI_REQUEST_JIT_UNDEFINED:
      {
        int const defined = (event->request == SPTI_REQUEST_JIT_DEFINED);
        spio_t_bits options;
        if (defined) {
          options = JIT_DEFINED_OPTION_DEFINED;
        } else {
          options = JIT_DEFINED_OPTION_UNDEFINED;
        }

        if (event->params_len >= 5) {
          int i = 0;
          char const *name = event->params[i++].str;
          SP_integer arity = event->params[i++].integer;
          char const *module = event->params[i++].str;
          void const *addr = event->params[i++].ptr;
          size_t size = event->params[i++].size;
          jit_defined(SPAPI_ARG
		      name, arity, module, addr, size, cookie, options);
        }
      }
      break;
    default:
      break;
    }
  code = SPIO_S_NOERR;

 cleanup:

  return code;
 barf:
  goto cleanup;
}

static spti_t_cookie spti_cookie;

static int sp_perf_disable = 0;

static void register_spti(tSP_install_spti_hook *install_spti_hook)
{
  spio_t_error_code code = SPIO_E_INTERNAL_ERROR;
  spio_t_error_code SPCDECL (*hook)(sp_t_spti_event *event, void *cookie_) = &spti_hook;

  SPIO_REFERENCE_BARF_LABEL();

  spti_cookie.inited = 0;

  /* [PM] 4.3.3 Mainly useful when building the perf library so it
     does not generate perf data in /tmp. */
  {
    char *tmp = getenv("SP_PERF_DISABLE"); /* NOTE: not a system property. */
    if (tmp != NULL && strcmp(tmp, "yes") == 0) {
      sp_perf_disable = 1;
    }
  }

  if (sp_perf_disable) {
    /* [PM] 4.3.3 We would like to do BARF(SPIO_E_NOT_SUPPORTED),
       but that would trigger all kinds of soft asserts in
       foreign.c. */
    hook = &spti_hook_dummy;
  }

  code = (*install_spti_hook)(hook, &spti_cookie, SPIO_OPTION_NONE);
  CHECK(code);
  spti_cookie.inited = 1;
 cleanup:
  (void)code;
  return;
 barf:
  goto cleanup;
}

static void unregister_spti(tSP_install_spti_hook *install_spti_hook)
{
  spio_t_error_code code = SPIO_E_INTERNAL_ERROR;
  spio_t_error_code SPCDECL (*hook)(sp_t_spti_event *event, void *cookie_) = &spti_hook;

  SPIO_REFERENCE_BARF_LABEL();

  if (sp_perf_disable) {
    /* [PM] 4.3.3 foreign.c barfs if uninstalled hook differs from installed hook. */
    hook = &spti_hook_dummy;
  }

  CHECK((*install_spti_hook)(hook, NULL, SP_INSTALL_SPTI_HOOK_OPTION_UNINSTALL));

  code = SPIO_S_NOERR;
 cleanup:
  spti_cookie.inited = 0;

  (void)code;
  return;
 barf:
  goto cleanup;
}
