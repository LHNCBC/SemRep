%%% -*- Mode: Prolog; Module: resgen; -*-
%%%
%%% Generate .o (ELF) and .res (Win32) from data files. Used by spld
%%% to pre-link with, e.g., .sav files.
%%%
%%% Created November 2000 Per Mildner

/*
Supports:

ELF .o files
Solaris (SPARC, x86) (Can 64bit Solaris read elf32 files?)
Linux (x86) (would perhaps work on FreBSD etc too)
IRIX (MIPS)

.res files
Win32

Needed: XCOFF (AIX), ECOFF (TRU64 Unix/Digital Unix), SOF? (HP-UX).
Note that HP-UX now supports ELF but only for 64bit code.
*/

/*
(This doc is not current)

We utilize that almost all of the file can be independent of the
actual data.

Layout of generated resource:

ELF Header:
  unsigned char	e_ident[EI_NIDENT];	// Magic number and other info
  Elf32_Half	e_type;			// Object file type
  Elf32_Half	e_machine;		// Architecture
  Elf32_Word	e_version;		// Object file version
  Elf32_Addr	e_entry;		// Entry point virtual address
  Elf32_Off	e_phoff;		// Program header table file offset
  Elf32_Off	e_shoff;		// Section header table file offset
  Elf32_Word	e_flags;		// Processor-specific flags
  Elf32_Half	e_ehsize;		// ELF header size in bytes
  Elf32_Half	e_phentsize;		// Program header table entry size
  Elf32_Half	e_phnum;		// Program header table entry count
  Elf32_Half	e_shentsize;		// Section header table entry size
  Elf32_Half	e_shnum;		// Section header table entry count
  Elf32_Half	e_shstrndx;		// Section header string table index

Section Header String Table:
 .rodata\0
 .strtab\0
 .symtab\0
 .shstrtab\0
 .comment\0 (optional, not yet generated)

Static Symbol Table .symtab1 (only needed if .comment used)
#0 UNDEF
#1 .comment

cSection Header Table
#0 UNDEF
#1 .shstrtab
#2 .symtab
#3 .strtab
< From here on things are resource specific>
#4 .rodata
#5 (.note)
#6 (.comment)

. Dynamic Symbol Table (for the generated resource) .symtab
 #0 UNDEF
 #1 st_name = file name (#1 in Dynamic String Table) STT_FILE...
 #2 st_name = resource name (#2 in Dynamic String Table) STB_GLOBAL...
. Dynamic String Table
\0
<FILE NAME>\0
<RESOURCE NAME>\0

. Section .rodata (data for the generated resource)
<RESOURCE INFO>\0
<RESOURCE DATA>
<EOF>
*/

:- module(resgen, [
                   spld_resgen_prepare_data_resource_prefix/3,
                   spld_resgen_prepare_data_resource_prefix_raw/3,
                   spld_file_to_cc/3,

                   write_resource_prefix/3,
                   spld_resgen_prepare_license/3
                   %% entry points useful for debugging:
                   %% write_elf_resource/2,
                   %% write_elf_resource/3,
                   %% write_elf_resource_prefix/4,
                   %% write_win32_resource/2,
                   %% write_win32_resource/3,
                   %% write_win32_resource_prefix/4
                  ]).


:- use_module(library(types), [
	illarg/4
	]).
:- use_module(library(lists), [
	nth0/3
	]).
:- use_module(library(file_systems), [
	file_property/3
	]).
%% :- use_module(library(system), [
%%         datime/2
%%    ]).

%% :- use_module(library(resource)).

:- meta_predicate barf(+,:).

%% These are neither used nor exported but may be useful when debugging
:- public write_elf_resource/2, write_elf_resource/3, write_win32_resource/2.

%% These are currently unused but may be needed in the future.
:- public elf_rel_entry_format/2.

spld_resgen_prepare_data_resource_prefix(SourcePathAtom, SourceName, Output) :-
   write_resource_prefix(SourcePathAtom, SourceName, Output), !.
spld_resgen_prepare_data_resource_prefix(SourcePathAtom, SourceName, Output) :-
   Goal=spld_resgen_prepare_data_resource_prefix(SourcePathAtom, SourceName, Output),
   format(user_error, 'Goal failed: ~q~n', [Goal]),
   halt(1).

%% Generate raw data file for further processing (by Win32 resource compiler or file_to_cc->C-compiler)
spld_resgen_prepare_data_resource_prefix_raw(SourcePathAtom, SourceName, Output) :-
   set_fake_host_type('raw'),   % [PM] 3.12.2 SPRM 8709
   write_resource_prefix(SourcePathAtom, SourceName, Output, spres), !.
spld_resgen_prepare_data_resource_prefix_raw(SourcePathAtom, SourceName, Output) :-
   Goal=spld_resgen_prepare_data_resource_prefix_raw(SourcePathAtom, SourceName, Output),
   format(user_error, 'Goal failed: ~q~n', [Goal]),
   halt(1).


%%handle_error_message(Message) :- 
%%   print_message(error, Message),
%%   halt(1).

%%spld_resgen_prepare_data_resource_prefix(SourcePathAtom, SourceName, Output) :-
%%   assert((user:portray_message(error,Message) :- handle_error_message(Message)), Ref),
%%   ( write_resource_prefix(SourcePathAtom, SourceName, Output) ->
%%       erase(Ref),
%%       true
%%   ; otherwise ->
%%       halt(1)
%%   ).

%% [PM] 3.12.2 SPRM 8709 RESGEN_TYPE=cc should not know about real platform.
%%             Instead fake it (as 'raw'). Also useful if/when adding
%%             cross compilation support.
:- dynamic fake_host_type/1.
:- volatile fake_host_type/1.

set_fake_host_type(HostType) :-
   retractall(fake_host_type(_)),
   assert(fake_host_type(HostType)).

host_type(HostType) :-
   fake_host_type(FakeHostType), !,
   HostType = FakeHostType.
host_type(HostType) :-
   prolog_flag(host_type, HostType).

write_resource_prefix(SourcePath, SourceName, Output) :-
   host_type(HostType),
   host_resource_kind(HostType, ResourceKind),
   write_resource_prefix(SourcePath, SourceName, Output, ResourceKind).


write_resource_prefix(SourcePath, SourceName, Output, ResourceKind) :-
   file_property(SourcePath, size_in_bytes, Size),
   %% file_property(SourcePath, modify_time, ModDatime), datime(ResourceMTime, ModDatime),
   file_property(SourcePath, modify_timestamp, ResourceMTime), % [PM] 4.0 new file property
   write_resource_prefix(ResourceKind, Size, ResourceMTime, SourceName, Output).


write_resource_prefix('elf', Size, ResourceMTime, SourceName, Output) :-
   sp_ensure_c_name(SourceName, HexName),
   atom_codes(HexName, HexCodes),
   append("SICStus_DATA_RESOURCE_", HexCodes, ResourceName),
   write_elf_resource_prefix(Size, ResourceMTime, Output, ResourceName).
write_resource_prefix('win32res', Size, ResourceMTime, SourceName, Output) :-
   atom_codes(SourceName, SourceNameCodes),
   write_win32_resource_prefix(Size, ResourceMTime, Output, SourceNameCodes).
write_resource_prefix('spres', Size, ResourceMTime, SourceName, Output) :-
   atom_codes(SourceName, SourceNameCodes),
   write_spres_resource_prefix(Size, ResourceMTime, Output, SourceNameCodes).
%% currently the same as spres (raw data file for further processing)
write_resource_prefix('cc', Size, ResourceMTime, SourceName, Output) :-
   atom_codes(SourceName, SourceNameCodes),
   write_spres_resource_prefix(Size, ResourceMTime, Output, SourceNameCodes).

host_os_type(HostType, OsType) :- var(HostType), !,
   Goal = host_os_type(HostType, OsType),
   illarg(var, Goal, 1, HostType).
host_os_type(HostType, OsType) :- var(OsType), !,
   Goal = host_os_type(HostType, OsType),
   illarg(var, Goal, 2, OsType).
host_os_type(HostType, OsType) :-
   once(sub_atom(HostType, _Before,_Length,_After, OsType)).

host_resource_kind(HostType,  ResourceKind) :-
   host_os_type(HostType, solaris), !,
   ResourceKind = elf.
host_resource_kind(HostType,  ResourceKind) :-
   host_os_type(HostType, linux), !,
   ResourceKind = elf.
host_resource_kind(HostType,  ResourceKind) :-
   host_os_type(HostType, irix), !,
   ResourceKind = elf.
host_resource_kind(HostType,  ResourceKind) :-
   host_os_type(HostType, win32), !,
   ResourceKind = spres.
host_resource_kind(_HostType,  ResourceKind) :-
   ResourceKind = cc.



:- dynamic elf_file_order_/1.
:- volatile elf_file_order_/1.


%% Work around spdet imprecision, elf_file_order_ is really determinate
elf_file_order(FileOrder) :-
   once(elf_file_order_(FileOrder)).

elf_default_setup('raw', Info) :- !,
   Info = info([]).
elf_default_setup(HostType, Info) :-
   elf_default_machine(HostType, E_MACHINE),

   elf_default_file_order(E_MACHINE, ELFDATA2),
   retractall(elf_file_order_(_)),
   assert(elf_file_order_(ELFDATA2)),

   elf_default_relocation_type(E_MACHINE, RELTYPE),
   elf_default_e_flags_expr(E_MACHINE, HostType, E_FLAGS_EXPR),
   Info = info(['e_machine'=E_MACHINE, 'elfdata2'=ELFDATA2, 'RelocationType'=RELTYPE, 'e_flags_expr'=E_FLAGS_EXPR]).


write_elf_resource(ResourceData, ResourceName) :-
   ResourceMTime = 0,                   % Hmm
   write_elf_resource(ResourceData, ResourceMTime, ResourceName).

write_elf_resource(ResourceData, ResourceMTime, ResourceName) :-
   host_type(HostType),
   append(ResourceName, ".o", FileName),
   atom_codes(Output, FileName),
   write_elf_resource(ResourceData, ResourceMTime, ResourceName, Output, HostType).

write_elf_resource(ResourceData, ResourceMTime, ResourceName, Output, HostType) :-
   ResourceInfo = data(ResourceData),
   write_elf_resource1(ResourceInfo, ResourceMTime, ResourceName, Output, HostType).


write_elf_resource_prefix(ResourceSize, ResourceMTime, Output, ResourceName) :-
   host_type(HostType),
   write_elf_resource_prefix1(ResourceSize, ResourceMTime, ResourceName, Output, HostType).

write_elf_resource_prefix1(ResourceSize, ResourceMTime, ResourceName, Output, HostType) :-
   ResourceInfo = size(ResourceSize),
   write_elf_resource1(ResourceInfo, ResourceMTime, ResourceName, Output, HostType).


write_elf_resource1(ResourceInfo, ResourceMTime, ResourceName, Output, HostType) :-
   elf_default_setup(HostType, Info),
   %% append(ResourceName, ".o", FileName),
   atom_codes(Output, FileName),
   open(Output, write, Stream, [type(binary)]),
   call_cleanup(
                write_elf_resource2(ResourceInfo, ResourceMTime, FileName, ResourceName, Stream, Info),
                close(Stream)
               ).

write_elf_resource2(ResourceInfo, ResourceMTime, FileName, ResourceName, Stream, Info) :-
   link_elf_resource_file(ResourceInfo, ResourceMTime, FileName, ResourceName, FileData, Info),
   write_elf_descs(FileData, Stream, Info).


elf_machine(info(INFO), E_MACHINE) :-
   memberchk('e_machine'=E_MACHINE0, INFO),
   E_MACHINE = E_MACHINE0.

elf_default_machine(HostType, E_MACHINE) :- !, % [PM] 3.9.0 Solaris i386 port. Use the new xxelf_default_machine 
   xxelf_default_machine(HostType, E_MACHINE).

xxelf_default_machine(HostType, E_MACHINE) :-
   elf_os(OsType),              % BT
   host_os_type(HostType, OsType),
   !,                           % This OS uses ELF
   atom_codes(HostType, HostTypeCodes),
   once(append(CPU_Codes, [0'-|_Rest],HostTypeCodes)), % CPU = x86, x86_64, i386, mips, sparc, sparc64, ...
   atom_codes(CPU, CPU_Codes),
   cpu_e_machine(CPU, E_MACHINE1),
   E_MACHINE = E_MACHINE1.
xxelf_default_machine(HostType, E_MACHINE) :- % not an ELF target
   host_os_type(HostType, win32),
   !,                           % Win32, uses resources
   E_MACHINE='WIN32'.           % faked EM_-type
xxelf_default_machine(HostType, E_MACHINE) :-
   E_MACHINE = unknown(HostType).

%% determinate in first arg, maps hosttype CPU to EM_XXX
cpu_e_machine(sparc, 'EM_SPARC').
cpu_e_machine(sparc64, 'EM_SPARC').
cpu_e_machine(i386,  'EM_386').
cpu_e_machine(x86,   'EM_386').
cpu_e_machine(x86_64, 'EM_386').
cpu_e_machine(mips,  'EM_MIPS').

%% OSes that use elf (HPUX support will be a problem, it has ELF only for 64bit code)
elf_os(solaris).
elf_os(linux).
elf_os(irix).

elf_default_file_order('EM_386', 'ELFDATA2LSB').
elf_default_file_order('EM_SPARC', 'ELFDATA2MSB').
elf_default_file_order('EM_MIPS', 'ELFDATA2MSB').
elf_default_file_order('WIN32', 'ELFDATA2LSB'). % Win 32 .res file (not elf)
elf_default_file_order(unknown(HostType), unknown(HostType)).

elf_rel_entry_format(info(Info), RelocationType) :-
   memberchk('RelocationType'=RelocationType, Info).

elf_default_relocation_type('EM_386', 'SHT_REL').
elf_default_relocation_type('EM_SPARC', 'SHT_RELA').
elf_default_relocation_type('EM_MIPS', 'SHT_REL').
elf_default_relocation_type('WIN32', 'NONE').
elf_default_relocation_type(unknown(HostType), unknown(HostType)).

elf_e_flags_expr(info(INFO), E_FLAGS_EXPR) :-
   memberchk('e_flags_expr'=E_FLAGS_EXPR0, INFO),
   E_FLAGS_EXPR = E_FLAGS_EXPR0.

elf_default_e_flags_expr('EM_MIPS', _HostType, E_FLAGS_EXPR) :- !,
   E_FLAGS_EXPR =
                  ((0
                   %% Added PIC after verifying .o format OK.
                   \/ const('EF_MIPS_PIC')
                   \/ const('EF_MIPS_ABI2')
                   )
                  %% + const('EF_MIPS_ARCH_4')
                  ).
elf_default_e_flags_expr(_E_MACHINE, _HostType, E_FLAGS_EXPR) :-
   E_FLAGS_EXPR = 0.

elf_type(Type, RawType) :-
   elf_type(Type, RawType, _Size).

elf_type(Type, RawType, Size) :-
   elf_basic_type(Type, RawType0, Size0), !,
   RawType = RawType0,
   Size = Size0.
elf_type(FieldName, RawType, Size) :-
   elf_field_type(FieldName, Type), !,
   elf_basic_type(Type, RawType, Size).



elf_basic_type('Elf32_Addr',    'u32', 4).
elf_basic_type('Elf32_Half',    'u16', 2).
elf_basic_type('Elf32_Off',     'u32', 4).
elf_basic_type('Elf32_Sword',   's32', 4).
elf_basic_type('Elf32_Word',    'u32', 4).
elf_basic_type('Elf32_Section', 'u16', 2).
elf_basic_type('unsigned char',  'u8', 1).
elf_basic_type('Win32_WORD',   's16', 2).
elf_basic_type('Win32_DWORD',   's32', 4).


%% Elf32_Ehdr 
elf_field_type(e_type, 'Elf32_Half').      % Object file type
elf_field_type(e_machine, 'Elf32_Half').   % Architecture
elf_field_type(e_version, 'Elf32_Word').   % Object file version
elf_field_type(e_entry, 'Elf32_Addr').     % Entry point virtual address
elf_field_type(e_phoff, 'Elf32_Off').      % Program header table file offset
elf_field_type(e_shoff, 'Elf32_Off').      % Section header table file offset
elf_field_type(e_flags, 'Elf32_Word').     % Processor-specific flags
elf_field_type(e_ehsize, 'Elf32_Half').    % ELF header size in bytes
elf_field_type(e_phentsize, 'Elf32_Half'). % Program header table entry size
elf_field_type(e_phnum, 'Elf32_Half').     % Program header table entry count
elf_field_type(e_shentsize, 'Elf32_Half'). % Section header table entry size
elf_field_type(e_shnum, 'Elf32_Half').     % Section header table entry count
elf_field_type(e_shstrndx, 'Elf32_Half').  % Section header string table index
%% Elf32_Shdr
elf_field_type(sh_name, 'Elf32_Word').     % Section name (string tbl index)
elf_field_type(sh_type, 'Elf32_Word').      % Section type
elf_field_type(sh_flags, 'Elf32_Word').     % Section flags
elf_field_type(sh_addr, 'Elf32_Addr').      % Section virtual addr at execution
elf_field_type(sh_offset, 'Elf32_Off').     % Section file offset
elf_field_type(sh_size, 'Elf32_Word').      % Section size in bytes
elf_field_type(sh_link, 'Elf32_Word').      % Link to another section
elf_field_type(sh_info, 'Elf32_Word').      % Additional section information
elf_field_type(sh_addralign, 'Elf32_Word'). % Section alignment
elf_field_type(sh_entsize, 'Elf32_Word').   % Entry size if section holds table
%% struct Elf32_Sym
elf_field_type(st_name, 'Elf32_Word').     % Symbol name (string tbl index)
elf_field_type(st_value, 'Elf32_Addr').    % Symbol value
elf_field_type(st_size, 'Elf32_Word').     % Symbol size
elf_field_type(st_info, 'unsigned char').  % Symbol type and binding
elf_field_type(st_other, 'unsigned char'). % No defined meaning, 0
elf_field_type(st_shndx, 'Elf32_Section'). % Section index
%% struct Elf32_Rel
elf_field_type(r_offset, 'Elf32_Addr').    % Address (section offset)
elf_field_type(r_info, 'Elf32_Word').      % Relocation type and symbol index
%% struct Elf32_Rela
elf_field_type(r_addend, 'Elf32_Sword').   % Addend

%% struct Elf32_RegInfo (MIPS)
elf_field_type(ri_gprmask, 'Elf32_Word').
elf_field_type(ri_cprmask, 'Elf32_Word'). % really ri_cprmask[4]
elf_field_type(ri_gp_value, 'Elf32_Sword').




elf_add_type_size(Type, Off0, Off) :-
   elf_type(Type, _RawType, Size),
   Off is Off0+Size.


write_elf_raw( 'u8', Value, S) :-
   write_elf_byte(Value, S).
write_elf_raw('u16', Value, S) :-
   write_elf_bytes(Value, 2, S).
write_elf_raw('s16', Value, S) :-
   write_elf_bytes(Value, 2, S).
write_elf_raw('u32', Value, S) :-
   write_elf_bytes(Value, 4, S).
write_elf_raw('s32', Value, S) :-
   write_elf_bytes(Value, 4, S).


write_elf_bytes(Value, Len, S) :-
   elf_file_order(Order),
   write_elf_bytes1(Order, Value, Len, S).

write_elf_bytes1('ELFDATA2LSB', Value, Len, S) :-
   write_elf_bytes_lsb(Len, Value, S).
write_elf_bytes1('ELFDATA2MSB', Value, Len, S) :-
   write_elf_bytes_msb(Len, Value, S).

write_elf_byte(Value, S) :-
   put_byte(S, Value).

write_elf_bytes_lsb(Len, _Value, _S) :- Len == 0, !,
   true.
write_elf_bytes_lsb(Len, Value, S) :- % Len > 0
   LSB is Value /\ 0xFF,
   write_elf_byte(LSB, S),
   Value1 is Value >> 8,
   Len1 is Len-1,
   write_elf_bytes_lsb(Len1, Value1, S).

write_elf_bytes_msb(Len, _Value, _S) :- Len == 0, !,
   true.
write_elf_bytes_msb(Len, Value, S) :- % Len > 0
   LSB is Value /\ 0xFF,
   Value1 is Value >> 8,
   Len1 is Len-1,
   write_elf_bytes_msb(Len1, Value1, S),
   write_elf_byte(LSB, S).


write_elf_value(ElfType, Value, S) :-
   elf_type(ElfType, RawType),
   write_elf_raw(RawType, Value, S).


write_elf_raw_vector(Bytes, S, _Info) :-
   elf_type('unsigned char', RawType),
   write_elf_raw_vector1(Bytes, RawType, S).

write_elf_raw_vector1([], _RawType, _S).
write_elf_raw_vector1([Byte|Bytes], RawType, S) :-
   write_elf_raw(RawType, Byte, S),
   write_elf_raw_vector1(Bytes, RawType, S).

write_elf_string(Bytes, S, _Info) :-
   elf_type('unsigned char', RawType),
   write_elf_raw_vector1(Bytes, RawType, S),
   write_elf_raw(RawType, 0, S).

write_win32_unicode_string(Chars, S, _Info) :-
   RawType = 'u16',
   write_elf_raw_vector1(Chars, 'u16', S),
   write_elf_raw(RawType, 0, S).

write_elf_desc(Desc, _S, _Info) :- var(Desc), !,
   throw(elf_error('Uninstantiated Desc')).
write_elf_desc(ElfType=Value, S, _Info) :-
   write_elf_value(ElfType, Value, S).
write_elf_desc(struct(Descs), S, Info) :-
   write_elf_descs(Descs, S, Info).
write_elf_desc(raw_vector(Bytes), S, Info) :-
   write_elf_raw_vector(Bytes, S, Info).
write_elf_desc(string(Bytes), S, Info) :-
   write_elf_string(Bytes, S, Info).
write_elf_desc(win32_unicode_string(Chars), S, Info) :-
   write_win32_unicode_string(Chars, S, Info).
write_elf_desc(const(Name), S, Info) :-
   elf_const(Name, Value, ElfType, Info),
   write_elf_value(ElfType, Value, S).
write_elf_desc(_Var is _Expr, _S, _Info) :-
   true.                        % no output
write_elf_desc(assert(_Expr,_), _S, _Info) :-
   true.                        % no output


write_elf_descs([], _S, _Info).
write_elf_descs([Desc|Values], S, Info) :-
   write_elf_desc(Desc, S, Info),
   write_elf_descs(Values, S, Info).

elf_const(Name, Value, Type, _Info) :-
   elf_const1(Name, Value0, Type0), !,
   Value = Value0,
   Type = Type0.
elf_const(Name, Value, Type, Info) :-
   elf_generic_const(Name, Value, Type, Info).


elf_const1('ELFDATA2LSB', 1, 'unsigned char').
elf_const1('ELFDATA2MSB', 2, 'unsigned char').

elf_const1('ELFCLASS32', 1, 'unsigned char').
elf_const1('ELFCLASS64', 2, 'unsigned char').

elf_const1('EV_CURRENT', 1, 'unsigned char').

elf_const1('ET_NONE', 0, 'Elf32_Half').     % No file type
elf_const1('ET_REL', 1, 'Elf32_Half').      % Relocatable file
elf_const1('ET_EXEC', 2, 'Elf32_Half').     % Executable file
elf_const1('ET_DYN', 3, 'Elf32_Half').      % Shared object file
elf_const1('ET_CORE', 4, 'Elf32_Half').     % Core file

elf_const1('SHF_WRITE', 1, 'Elf32_Word').
elf_const1('SHF_ALLOC', 2, 'Elf32_Word').
elf_const1('SHF_EXECINSTR', 4, 'Elf32_Word').

elf_const1('SHN_ABS', 0xFFF1, 'Elf32_Half').
elf_const1('SHN_COMMON', 0xFFF2, 'Elf32_Half').

elf_const1('SHT_NULL',0,'Elf32_Word').
elf_const1('SHT_PROGBITS',1,'Elf32_Word').
elf_const1('SHT_SYMTAB',2,'Elf32_Word').
elf_const1('SHT_STRTAB',3,'Elf32_Word').
elf_const1('SHT_RELA',4,'Elf32_Word').
elf_const1('SHT_HASH',5,'Elf32_Word').
elf_const1('SHT_DYNAMIC',6,'Elf32_Word').
elf_const1('SHT_NOTE',7,'Elf32_Word').
elf_const1('SHT_NOBITS',8,'Elf32_Word').
elf_const1('SHT_REL',9,'Elf32_Word').
elf_const1('SHT_SHLIB',10,'Elf32_Word').
elf_const1('SHT_DYNSYM',11,'Elf32_Word').

elf_const1('STB_LOCAL',0,'unsigned char').
elf_const1('STB_GLOBAL',1,'unsigned char').
elf_const1('STB_WEAK',2,'unsigned char').

elf_const1('STT_NOTYPE',0,'unsigned char').
elf_const1('STT_OBJECT',1,'unsigned char').
elf_const1('STT_FUNC',2,'unsigned char').
elf_const1('STT_SECTION',3,'unsigned char').
elf_const1('STT_FILE',4,'unsigned char').
elf_const1('STT_NUM',5,'unsigned char').

elf_const1('EM_NONE', 0, 'Elf32_Half').              % No machine
elf_const1('EM_M32', 1, 'Elf32_Half').               % AT&T WE 32100
elf_const1('EM_SPARC', 2, 'Elf32_Half').             % SUN SPARC
elf_const1('EM_386', 3, 'Elf32_Half').               % Intel 80386
elf_const1('EM_68K', 4, 'Elf32_Half').               % Motorola m68k family
elf_const1('EM_88K', 5, 'Elf32_Half').               % Motorola m88k family
elf_const1('EM_486', 6, 'Elf32_Half').               % Intel 80486
elf_const1('EM_860', 7, 'Elf32_Half').               % Intel 80860
elf_const1('EM_MIPS', 8, 'Elf32_Half').              % MIPS R3000 big-endian
elf_const1('EM_S370', 9, 'Elf32_Half').              % Amdahl
elf_const1('EM_MIPS_RS4_BE', 10, 'Elf32_Half').      % MIPS R4000 big-endian
elf_const1('EM_RS6000', 11, 'Elf32_Half').           % RS6000

elf_const1('EM_PARISC', 15, 'Elf32_Half').           % HPPA
elf_const1('EM_nCUBE', 16, 'Elf32_Half').            % nCUBE
elf_const1('EM_VPP500', 17, 'Elf32_Half').           % Fujitsu VPP500
elf_const1('EM_SPARC32PLUS', 18, 'Elf32_Half').      % Sun's "v8plus"
elf_const1('EM_960', 19, 'Elf32_Half').              % Intel 80960
elf_const1('EM_PPC', 20, 'Elf32_Half').              % PowerPC

elf_const1('EM_V800', 36, 'Elf32_Half').             % NEC V800 series
elf_const1('EM_FR20', 37, 'Elf32_Half').             % Fujitsu FR20
elf_const1('EM_RH32', 38, 'Elf32_Half').             % TRW RH32
elf_const1('EM_MMA', 39, 'Elf32_Half').              % Fujitsu MMA
elf_const1('EM_ARM', 40, 'Elf32_Half').              % ARM
elf_const1('EM_FAKE_ALPHA', 41, 'Elf32_Half').       % Digital Alpha
elf_const1('EM_SH', 42, 'Elf32_Half').               % Hitachi SH
elf_const1('EM_SPARCV9', 43, 'Elf32_Half').          % SPARC v9 64-bit
elf_const1('EM_TRICORE', 44, 'Elf32_Half').          % Siemens Tricore
elf_const1('EM_ARC', 45, 'Elf32_Half').              % Argonaut RISC Core
elf_const1('EM_H8_300', 46, 'Elf32_Half').           % Hitachi H8/300
elf_const1('EM_H8_300H', 47, 'Elf32_Half').          % Hitachi H8/300H
elf_const1('EM_H8S', 48, 'Elf32_Half').              % Hitachi H8S
elf_const1('EM_H8_500', 49, 'Elf32_Half').           % Hitachi H8/500
elf_const1('EM_IA_64', 50, 'Elf32_Half').            % Intel Merced
elf_const1('EM_MIPS_X', 51, 'Elf32_Half').           % Stanford MIPS-X
elf_const1('EM_COLDFIRE', 52, 'Elf32_Half').         % Motorola Coldfire
elf_const1('EM_68HC12', 53, 'Elf32_Half').           % Motorola M68HC12

%%% 386 Relocation
elf_const1('R_386_NONE', 0, 'Elf32_Word'). % No reloc
elf_const1('R_386_32', 1, 'Elf32_Word').             % Direct 32 bit 
elf_const1('R_386_PC32', 2, 'Elf32_Word').           % PC relative 32 bit
elf_const1('R_386_GOT32', 3, 'Elf32_Word').          % 32 bit GOT entry
elf_const1('R_386_PLT32', 4, 'Elf32_Word').          % 32 bit PLT address
elf_const1('R_386_COPY', 5, 'Elf32_Word').           % Copy symbol at runtime
elf_const1('R_386_GLOB_DAT', 6, 'Elf32_Word').       % Create GOT entry
elf_const1('R_386_JMP_SLOT', 7, 'Elf32_Word').       % Create PLT entry
elf_const1('R_386_RELATIVE', 8, 'Elf32_Word').       % Adjust by program base
elf_const1('R_386_GOTOFF', 9, 'Elf32_Word').         % 32 bit offset to GOT
elf_const1('R_386_GOTPC', 10, 'Elf32_Word').         % 32 bit PC relative offset to GOT
%%% SPARC Relocation
elf_const1('R_SPARC_NONE',0, 'Elf32_Word').
elf_const1('R_SPARC_8',1, 'Elf32_Word').
elf_const1('R_SPARC_16',2, 'Elf32_Word').
elf_const1('R_SPARC_32',3, 'Elf32_Word').
elf_const1('R_SPARC_DISP8',4, 'Elf32_Word').
elf_const1('R_SPARC_DISP16',5, 'Elf32_Word').
elf_const1('R_SPARC_DISP32',6, 'Elf32_Word').
elf_const1('R_SPARC_WDISP30',7, 'Elf32_Word').
elf_const1('R_SPARC_WDISP22',8, 'Elf32_Word').
elf_const1('R_SPARC_HI22',9, 'Elf32_Word').
elf_const1('R_SPARC_22',10, 'Elf32_Word').
elf_const1('R_SPARC_13',11, 'Elf32_Word').
elf_const1('R_SPARC_LO10',12, 'Elf32_Word').
elf_const1('R_SPARC_GOT10',13, 'Elf32_Word').
elf_const1('R_SPARC_GOT13',14, 'Elf32_Word').
elf_const1('R_SPARC_GOT22',15, 'Elf32_Word').
elf_const1('R_SPARC_PC10',16, 'Elf32_Word').
elf_const1('R_SPARC_PC22',17, 'Elf32_Word').
elf_const1('R_SPARC_WPLT30',18, 'Elf32_Word').
elf_const1('R_SPARC_COPY',19, 'Elf32_Word').
elf_const1('R_SPARC_GLOB_DAT',20, 'Elf32_Word').
elf_const1('R_SPARC_JMP_SLOT',21, 'Elf32_Word').
elf_const1('R_SPARC_RELATIVE',22, 'Elf32_Word').
elf_const1('R_SPARC_UA32',23, 'Elf32_Word').
elf_const1('R_SPARC_PLT32',24, 'Elf32_Word').
elf_const1('R_SPARC_HIPLT22',25, 'Elf32_Word').
elf_const1('R_SPARC_LOPLT10',26, 'Elf32_Word').
elf_const1('R_SPARC_PCPLT32',27, 'Elf32_Word').
elf_const1('R_SPARC_PCPLT22',28, 'Elf32_Word').
elf_const1('R_SPARC_PCPLT10',29, 'Elf32_Word').
elf_const1('R_SPARC_10',30, 'Elf32_Word').
elf_const1('R_SPARC_11',31, 'Elf32_Word').
elf_const1('R_SPARC_64',32, 'Elf32_Word').
elf_const1('R_SPARC_OLO10',33, 'Elf32_Word').
elf_const1('R_SPARC_HH22',34, 'Elf32_Word').
elf_const1('R_SPARC_HM10',35, 'Elf32_Word').
elf_const1('R_SPARC_LM22',36, 'Elf32_Word').
elf_const1('R_SPARC_PC_HH22',37, 'Elf32_Word').
elf_const1('R_SPARC_PC_HM10',38, 'Elf32_Word').
elf_const1('R_SPARC_PC_LM22',39, 'Elf32_Word').
elf_const1('R_SPARC_WDISP16',40, 'Elf32_Word').
elf_const1('R_SPARC_WDISP19',41, 'Elf32_Word').
elf_const1('R_SPARC_GLOB_JMP',42, 'Elf32_Word').
elf_const1('R_SPARC_7',43, 'Elf32_Word').
elf_const1('R_SPARC_5',44, 'Elf32_Word').
elf_const1('R_SPARC_6',45, 'Elf32_Word').
elf_const1('R_SPARC_DISP64',46, 'Elf32_Word').
elf_const1('R_SPARC_PLT64',47, 'Elf32_Word').
elf_const1('R_SPARC_HIX22',48, 'Elf32_Word').
elf_const1('R_SPARC_LOX10',49, 'Elf32_Word').
elf_const1('R_SPARC_H44',50, 'Elf32_Word').
elf_const1('R_SPARC_M44',51, 'Elf32_Word').
elf_const1('R_SPARC_L44',52, 'Elf32_Word').
elf_const1('R_SPARC_REGISTER',53, 'Elf32_Word').
elf_const1('R_SPARC_UA64',54, 'Elf32_Word').
elf_const1('R_SPARC_UA16',55, 'Elf32_Word').

%% MIPS (IRIX 6.5)

elf_const1('EF_MIPS_NOREORDER',     0x00000001, 'Elf32_Word').
elf_const1('EF_MIPS_PIC',           0x00000002, 'Elf32_Word').
elf_const1('EF_MIPS_CPIC',          0x00000004, 'Elf32_Word').
elf_const1('EF_MIPS_XGOT',          0x00000008, 'Elf32_Word').
elf_const1('EF_MIPS_64BIT_WHIRL',   0x00000010, 'Elf32_Word').
elf_const1('EF_MIPS_ABI2',          0x00000020, 'Elf32_Word'). 
elf_const1('EF_MIPS_OPTIONS_FIRST', 0x00000080, 'Elf32_Word'). 

elf_const1('EF_MIPS_ARCH',          0xf0000000, 'Elf32_Word').	% mask: 4 bit field
elf_const1('EF_MIPS_ARCH_1',        0x00000000, 'Elf32_Word').
elf_const1('EF_MIPS_ARCH_2',        0x10000000, 'Elf32_Word').
elf_const1('EF_MIPS_ARCH_3',        0x20000000, 'Elf32_Word').
elf_const1('EF_MIPS_ARCH_4',        0x30000000, 'Elf32_Word').
elf_const1('EF_MIPS_ARCH_5',        0x40000000, 'Elf32_Word').
elf_const1('EF_MIPS_ARCH_6',        0x50000000, 'Elf32_Word').

elf_const1('EF_MIPS_ARCH_ASE',      0x0f000000, 'Elf32_Word').	% mask: 4 bit field
elf_const1('EF_MIPS_ARCH_ASE_MDMX', 0x08000000, 'Elf32_Word').	% multi-media extensions
elf_const1('EF_MIPS_ARCH_ASE_M16',  0x04000000, 'Elf32_Word').	% MIPS16 isa extensions

%% #define SHT_MIPS_LIBLIST	(SHT_LOPROC + 0)
%% #define SHT_MIPS_MSYM		(SHT_LOPROC + 1)
%% #define SHT_MIPS_CONFLICT	(SHT_LOPROC + 2)
%% #define SHT_MIPS_GPTAB		(SHT_LOPROC + 3)
%% #define SHT_MIPS_UCODE		(SHT_LOPROC + 4)
%% #define SHT_MIPS_DEBUG          (SHT_LOPROC + 5)
elf_const1('SHT_MIPS_REGINFO', 0x70000006, 'Elf32_Word'). % (SHT_LOPROC + 6)
%% #ifdef __osf__
%% #define	SHT_MIPS_PACKAGE	(SHT_LOPROC + 7)
%% #define	SHT_MIPS_PACKSYM	(SHT_LOPROC + 8)
%% #endif /* __osf__ */
%%
%% #define SHT_MIPS_RELD		(SHT_LOPROC + 9)
%% #define SHT_MIPS_DONTUSE	(SHT_LOPROC + 10)
%% /* Don't  use 10 until after the ragnarok beta */
%% #define SHT_MIPS_IFACE		(SHT_LOPROC + 11)
%% #define SHT_MIPS_CONTENT	(SHT_LOPROC + 12)
%% #define SHT_MIPS_OPTIONS	(SHT_LOPROC + 13)
%%
%% #define SHT_MIPS_SHDR		(SHT_LOPROC + 16)
%% #define SHT_MIPS_FDESC		(SHT_LOPROC + 17)
%% #define SHT_MIPS_EXTSYM		(SHT_LOPROC + 18)
%% #define SHT_MIPS_DENSE		(SHT_LOPROC + 19)
%% #define SHT_MIPS_PDESC		(SHT_LOPROC + 20)
%% #define SHT_MIPS_LOCSYM		(SHT_LOPROC + 21)
%% #define SHT_MIPS_AUXSYM		(SHT_LOPROC + 22)
%% #define SHT_MIPS_OPTSYM		(SHT_LOPROC + 23)
%% #define SHT_MIPS_LOCSTR		(SHT_LOPROC + 24)
%% #define SHT_MIPS_LINE		(SHT_LOPROC + 25)
%% #define SHT_MIPS_RFDESC		(SHT_LOPROC + 26)
%%
%% #define SHT_MIPS_DELTASYM	(SHT_LOPROC + 27)
%% #define SHT_MIPS_DELTAINST	(SHT_LOPROC + 28)
%% #define SHT_MIPS_DELTACLASS	(SHT_LOPROC + 29)
%%
%% #define SHT_MIPS_DWARF		(SHT_LOPROC + 30)
%% #define SHT_MIPS_DELTADECL	(SHT_LOPROC + 31)
%% #define SHT_MIPS_SYMBOL_LIB	(SHT_LOPROC + 32)
%% #define SHT_MIPS_EVENTS        	(SHT_LOPROC + 33)
%% #define SHT_MIPS_TRANSLATE     	(SHT_LOPROC + 34)
%% #define SHT_MIPS_PIXIE     	(SHT_LOPROC + 35)
%% #define SHT_MIPS_XLATE		(SHT_LOPROC + 36)
%% #define SHT_MIPS_XLATE_DEBUG	(SHT_LOPROC + 37)
%% #define SHT_MIPS_WHIRL		(SHT_LOPROC + 38)
%% #define SHT_MIPS_EH_REGION	(SHT_LOPROC + 39)
%% #define SHT_MIPS_XLATE_OLD	(SHT_LOPROC + 40)
%% #define SHT_MIPS_PDR_EXCEPTION	(SHT_LOPROC + 41)

%% Win32 .res file generation
elf_const1('RT_RCDATA', 10, 'Win32_DWORD').

elf_generic_const('R_ADDRESS', Value, Type, Info) :-
   elf_machine(Info, E_MACHINE),
   elf_const_r_address(E_MACHINE, Value, Type, Info).

elf_const_r_address('EM_386', Value, Type, _Info) :-
   elf_const1('R_386_32', Value, Type).
elf_const_r_address('EM_SPARC', Value, Type, _Info) :-
   elf_const1('R_SPARC_32', Value, Type).



link_elf_description(Desc, Data, Info) :-
   Offset0=0,
   link_elf_desc(Desc, Data,[], Offset0,_Offset, Constraints,[], Info),
   link_elf_constraints(Constraints, Info).

link_elf_desc(Desc, D,DT,Off0,Off,C,CT, Info) :- var(Desc), !,
   Culprit = link_elf_desc(Desc, D,DT,Off0,Off,C,CT, Info),
   barf('Instantiation error: ~w', [Culprit]).
link_elf_desc(struct(Descs), D,DT, Off0,Off, C,CT, Info) :-
   link_elf_desc_list(Descs, D,DT, Off0,Off, C,CT, Info).
%% link_elf_desc(Type=Val, [Type=Val|DT],DT, Off0,Off, C,CT, _Info) :-
%%    C = CT,
%%   elf_add_type_size(Type, Off0, Off).

%% Allow exprssions
link_elf_desc(Type=Expr, [Type=Val|DT],DT, Off0,Off, C,CT, _Info) :-
   Constraint = (Val is Expr),
   C = [offset(Off0),Constraint|CT],
   elf_add_type_size(Type, Off0, Off).
link_elf_desc(string(Bytes), [string(Bytes)|DT],DT, Off0,Off, C,CT, _Info) :-
   C = CT,
   length(Bytes, Size),
   Off is Off0+Size+1.                     % +1 for terminating NUL byte
link_elf_desc(win32_unicode_string(Chars), [win32_unicode_string(Chars)|DT],DT, Off0,Off, C,CT, _Info) :-
   C = CT,
   length(Chars, Len),
   Off is Off0+(Len*2)+2.                     % +2 for terminating NUL UNICODE char
link_elf_desc(raw_vector(Bytes), [raw_vector(Bytes)|DT],DT, Off0,Off, C,CT, _Info) :-
   C = CT,
   length(Bytes, Size),
   Off is Off0+Size.
link_elf_desc(const(Name), [Type=Val|DT],DT, Off0,Off, C,CT, Info) :-
   C = CT,
   elf_const(Name, Val, Type, Info),
   elf_add_type_size(Type, Off0, Off).
link_elf_desc(const(Name, Type), [Type=Val|DT],DT, Off0,Off, C,CT, Info) :-
   C = CT,
   elf_const(Name, Val, _Type, Info),
   elf_add_type_size(Type, Off0, Off).
link_elf_desc(size(Size), D,DT, Off0,Off, C,CT, _Info) :-
   C = CT,
   D = DT,
   Off is Off0+Size.
link_elf_desc(align(Type), D,DT, Off0,Off, C,CT, Info) :-
   elf_type(Type, _RawType, Size),
   Align is Off0 mod Size,
   ( Align == 0 ->
       C = CT,
       D = DT,
       Off is Off0+Size
   ; otherwise ->
       make_list(Align, 0, Padding),
       link_elf_desc(raw_vector(Padding), D,DT, Off0,Off, C,CT, Info)
   ).

link_elf_desc(Constraint, D,DT, Off0,Off, C,CT, _Info) :- Constraint = (_ is _),
   C = [offset(Off0),Constraint|CT],
   D = DT,
   Off = Off0.
link_elf_desc(Assertion, D,DT, Off0,Off, C,CT, _Info) :- Assertion = assert(_,_),
   C = [offset(Off0),Assertion|CT],
   D = DT,
   Off = Off0.

link_elf_desc_list(Descs, D,DT, Off0,Off, C,CT, Info) :- var(Descs), !,
   Culprit = link_elf_desc_list(Descs, D,DT, Off0,Off, C,CT, Info),
   barf('Instantiation error: ~w', [Culprit]).
link_elf_desc_list([], D,DT, Off0,Off, C,CT, _Info) :-
   C = CT,
   D = DT,
   Off = Off0.
link_elf_desc_list([Desc|Descs], D,DT, Off0,Off, C,CT, Info) :-
   link_elf_desc(Desc, D,DT1, Off0,Off1, C,CT1, Info),
   link_elf_desc_list(Descs, DT1,DT, Off1,Off, CT1,CT, Info).


make_list(N, _Elt, L) :- N == 0, !,
   L = [].
make_list(N, Elt, [Elt|L]) :-
   N1 is N-1,
   make_list(N1, Elt, L).

link_elf_constraints([], _Info).
link_elf_constraints(Constraints, Info) :- Constraints = [_|_],
   Offset = 'illegal',
   link_elf_constraints_step(Constraints, Unresolved,[], Offset, FlagU,FlagR, Info),
   ( FlagR == true ->
       ( FlagU == true ->                    % more to do
           link_elf_constraints(Unresolved, Info)
       ; otherwise ->
           true
       )
   ; otherwise ->                          % no progress
       barf('Stale constraints: ~q', [Unresolved])
   ).

link_elf_constraints_step([], L,LT, _Offset, _FlagU,_FlagR, _Info) :-
   L = LT.
link_elf_constraints_step([Constraint|Constraints], L,LT, Offset0, FlagU,FlagR, Info) :-
   link_elf_constraint(Constraint, L,LT1, Offset0,Offset, FlagU,FlagR, Info),
   link_elf_constraints_step(Constraints, LT1,LT, Offset, FlagU,FlagR, Info).

link_elf_constraint(Constraint, L,LT, Off0,Off, FlagU,FlagR, Info) :- Constraint = (X is Expr),
   ( ground(Expr) ->
       FlagR = true,
       L = LT,
       Off = Off0,
       eval_elf_expr(Expr, X, Off0, Info)
   ; otherwise ->                          % not evaluable yet
       FlagU = true,
       L = [Constraint|LT],
       Off = Off0
   ).
link_elf_constraint(Constraint, L,LT, Off0,Off, FlagU,FlagR, Info) :- Constraint = assert(Expr,_),
   ( ground(Expr) ->
       L = LT,
       FlagR = true,
       ( eval_elf_bool_expr(Expr, Off0, Info) ->
           Off=Off0
       ; otherwise ->
           barf('Assertion failed: ~q', [Constraint])
       )
   ; otherwise ->                          % not evaluable yet
       FlagU = true,
       L = [Constraint|LT],
       Off = Off0
   ).
link_elf_constraint(Constraint, L,LT, _Off0,Off, _FlagU,_FlagR, _Info) :- Constraint = offset(Offset),
   L = [Constraint|LT],
   Off = Offset.                           % new offset info

eval_elf_expr(Expr, Value, Here, Info) :- var(Expr), !,
   Culprit = eval_elf_expr(Expr, Value, Here, Info),
   barf('Instantiation error: ~w', [Culprit]).
eval_elf_expr(Expr, Value, _Here, _Info) :- integer(Expr), !,
   Value = Expr.
eval_elf_expr('here', Value, Here, _Info) :-
   Value = Here.
eval_elf_expr(X+Y, Value, Here, Info) :-
   eval_elf_expr(X, XV, Here, Info),
   eval_elf_expr(Y, YV, Here, Info),
   Value is XV+YV.
eval_elf_expr(X-Y, Value, Here, Info) :-
   eval_elf_expr(X, XV, Here, Info),
   eval_elf_expr(Y, YV, Here, Info),
   Value is XV-YV.
eval_elf_expr(X*Y, Value, Here, Info) :-
   eval_elf_expr(X, XV, Here, Info),
   eval_elf_expr(Y, YV, Here, Info),
   Value is XV*YV.
eval_elf_expr(X\/Y, Value, Here, Info) :-
   eval_elf_expr(X, XV, Here, Info),
   eval_elf_expr(Y, YV, Here, Info),
   Value is XV\/YV.
eval_elf_expr(X/\Y, Value, Here, Info) :-
   eval_elf_expr(X, XV, Here, Info),
   eval_elf_expr(Y, YV, Here, Info),
   Value is XV/\YV.
eval_elf_expr(X/Y, Value, Here, Info) :-
   eval_elf_expr(X, XV, Here, Info),
   eval_elf_expr(Y, YV, Here, Info),
   ( YV == 0 ->
       Culprit = eval_elf_expr(X/Y, Value, Here, Info),
       barf('Division by zero: ~w', [Culprit])
   ; otherwise ->
       Value0 is XV//YV,
       ( rem(XV,YV) =:= 0 ->
           Value = Value0
       ; otherwise ->                      % remainder
           barf('Uneven division ~q (~q)', [X/Y, XV/YV])
       )
   ).
eval_elf_expr(const(Name), Value, _Here, Info) :-
   elf_const(Name, Value, _Type, Info).
eval_elf_expr(st_info(Bind,Type), Value, Here, Info) :-
   eval_elf_expr(Bind, BindV, Here, Info),
   eval_elf_expr(Type, TypeV, Here, Info),
   Value is BindV<<4 + (TypeV/\0xF).
eval_elf_expr(r_info(SymbolIndex,Type), Value, Here, Info) :-
   eval_elf_expr(SymbolIndex, SymbolIndexV, Here, Info),
   eval_elf_expr(Type, TypeV, Here, Info),
   Value is SymbolIndexV<<8 + (TypeV/\0xFF).


eval_elf_bool_expr(Expr, Here, Info) :- var(Expr), !,
   Culprit = eval_elf_bool_expr(Expr, Here, Info),
   barf('Instantiation error: ~w', [Culprit]).
eval_elf_bool_expr(X == Y, Here, Info) :-
   eval_elf_expr(X, XV, Here, Info),
   eval_elf_expr(Y, YV, Here, Info),
   XV == YV.
eval_elf_bool_expr(aligned(Type), Here, _Info) :-
   elf_type(Type, _RawType, Size),
   (Here mod Size) =:= 0.

link_elf_resource_file(What, ResourceMTime, FILE_NAME, RESOURCE_NAME, FileData, Info) :- What = data(ResourceData),
   ResourceDataTemplate = raw_vector(ResourceData),
   resource_header_template(What, ResourceMTime, ResourceHeaderTemplate),
   link_elf_resource_file1(struct([ResourceHeaderTemplate, ResourceDataTemplate]), FILE_NAME, RESOURCE_NAME, FileData, Info).

link_elf_resource_file(What, ResourceMTime, FILE_NAME, RESOURCE_NAME, FileData, Info) :- What = size(ResourceSize),
   ResourceDataTemplate = size(ResourceSize),
   resource_header_template(What, ResourceMTime, ResourceHeaderTemplate),
   link_elf_resource_file1(struct([ResourceHeaderTemplate, ResourceDataTemplate]), FILE_NAME, RESOURCE_NAME, FileData, Info).

resource_header_template(data(ResourceData), ResourceMTime, Template) :-
   length(ResourceData, ResourceSize),
   resource_header_template1(ResourceSize, ResourceMTime, Template).
resource_header_template(size(ResourceSize), ResourceMTime, Template) :-
   resource_header_template1(ResourceSize, ResourceMTime, Template).


%% xref url.c
resource_header_template1(ResourceSize, ResourceMTime,
                          struct([
                                  raw_vector("SICStus Resource"),
                                  raw_vector(SizeBytes),
                                  raw_vector(TimeBytes)])) :-
   resource_size_bytes(ResourceSize, SizeBytes),
   resource_mtime_bytes(ResourceMTime, TimeBytes).

resource_mtime_bytes(ResourceMTime, TimeBytes) :-
   resource_int_bytes1(8, ResourceMTime, TimeBytes, []).

resource_size_bytes(ResourceSize, SizeBytes) :-
   resource_int_bytes1(8, ResourceSize, SizeBytes, []).

resource_int_bytes1(N, _ResourceSize, SizeBytes, SBT) :- N == 0, !,
   SizeBytes = SBT.
resource_int_bytes1(N, ResourceSize, SizeBytes, SBT) :-
   Byte is ResourceSize /\ 0xFF,
   ResourceSize1 is ResourceSize >> 8,
   N1 is N-1,
   resource_int_bytes1(N1, ResourceSize1, SizeBytes, [Byte|SBT]).

link_elf_resource_file1(ResourceDataTemplate, FILE_NAME, RESOURCE_NAME, FileData, Info) :-
   elf_resource_file_template(ResourceDataTemplate, FILE_NAME, RESOURCE_NAME, FileTemplate, Info),
   link_elf_description(FileTemplate, FileData, Info).


elf_resource_file_template(ResourceDataTemplate, FILE_NAME, RESOURCE_NAME, FileTemplate, Info) :-
   elf_machine(Info, E_MACHINE),
   elf_e_flags_expr(Info, E_FLAGS_EXPR),
   %% Comment blocks preceeded by [RELOC] was used to generate an object file corresponding to:
   %% const char * const <RESOURCE NAME> = "<RESOURCE DATA>"
   %% Instead we now generate as if compiling:
   %% const char <RESOURCE NAME>[] = "<RESOURCE DATA>"
   %% One advantage is that we no longer need relocation entries at all (!).
   %% [RELOC] elf_rel_entry_format(Info, RelocationType),
   FileTemplate =
   struct([
           ELFHeaderTemplate,
           StaticStringTableTemplate,
           SectionHeaderTableTemplate,
           DynamicSymbolTableTemplate,
           DynamicStringTableTemplate,
           %% [RELOC] RelocSectionTemplate,
           ResourceSectionTemplate
          ]),
   elf_file_order(ELFDATA2),
   ELFHeaderTemplate =
   struct([
           ELF_HEADER_OFFSET is here, % offset zero
           struct([             % e_ident
                      E_IDENT_START is here,
                      'unsigned char'=0x7F,
                      'unsigned char'=0'E,
                      'unsigned char'=0'L,
                                'unsigned char'=0'F,
                                const('ELFCLASS32'),
                      const(ELFDATA2),
                      const('EV_CURRENT'),
                      'unsigned char'=0,        % e_ident[EI_OSABI] 7 (in linux elf.h but not in ELF spec)
                      'unsigned char'=0,        % e_ident[EI_ABIVERSION] 8 (in linux elf.h but not in ELF spec)
                      'unsigned char'=0,
                      'unsigned char'=0,
                      'unsigned char'=0,
                      'unsigned char'=0,
                      'unsigned char'=0,
                      'unsigned char'=0,
                      'unsigned char'=0,
                      assert((here-E_IDENT_START) == 16, '(here-E_IDENT_START) == 16')
                     ]),
              const('ET_REL'),  % e _type
              const(E_MACHINE),
              const('EV_CURRENT', e_version), % e_version
              e_entry=0,        % Entry point virtual address
              e_phoff=0,        % Program header table file offset
              e_shoff=SECTION_HEADER_TABLE_FILE_OFFSET, % Section header table file offset
              e_flags=E_FLAGS,        % Processor-specific flags
              E_FLAGS is E_FLAGS_EXPR,
              e_ehsize=ELF_HEADER_SIZE, % ELF header size in bytes
              e_phentsize=0,    % Program header table entry size
              e_phnum=0,        % Program header table entry count
              e_shentsize=SECTION_HEADER_TABLE_ENTRY_SIZE, % Section header table entry size
              e_shnum=SECTION_HEADER_TABLE_ENTRY_COUNT, % Section header table entry count
              e_shstrndx=SECTION_HEADER_STRING_TABLE_INDEX, % Section header string table index
              ELF_HEADER_SIZE is here-ELF_HEADER_OFFSET,
              assert(ELF_HEADER_SIZE==52, 'ELF_HEADER_SIZE==52')
              ]),

   %% .shstrtab String Table Section
   StaticStringTableTemplate =
      struct([
              STATIC_STRING_TABLE_FILE_OFFSET is here,
              'unsigned char'=0,
              RODATA_STRING_TABLE_INDEX is (here-STATIC_STRING_TABLE_FILE_OFFSET),
              string(".rodata"),
              %% [RELOC] RELRODATA_STRING_TABLE_INDEX is (here-STATIC_STRING_TABLE_FILE_OFFSET),
              %% [RELOC] string(RELRODATA_STRING),
              STRTAB_STRING_TABLE_INDEX is (here-STATIC_STRING_TABLE_FILE_OFFSET),
              string(".strtab"),
              SYMTAB_STRING_TABLE_INDEX is (here-STATIC_STRING_TABLE_FILE_OFFSET),
              string(".symtab"),
              SHSTRTAB_STRING_TABLE_INDEX is (here-STATIC_STRING_TABLE_FILE_OFFSET),
              string(".shstrtab"),

              REGINFO_STRING_TABLE_INDEX is (here-STATIC_STRING_TABLE_FILE_OFFSET),
              string(".reginfo"),

              %% Later perhaps: .comment
              STATIC_STRING_TABLE_SIZE is (here-STATIC_STRING_TABLE_FILE_OFFSET)
             ]),
   SectionHeaderTableTemplate =
      struct([
              SECTION_HEADER_TABLE_FILE_OFFSET is here,
              % SHN_UNDEF
              struct([
                      sh_name=0, % Section name (string tbl index)
                      sh_type=0, % Section type
                      sh_flags=0, % Section flags
                      sh_addr=0, % Section virtual addr at execution
                      sh_offset=0, % Section file offset
                      sh_size=0, % Section size in bytes
                      sh_link=0, % Link to another section
                      sh_info=0, % Additional section information
                      sh_addralign=0, % Section alignment
                      sh_entsize=0 % Entry size if section holds table
                     ]),
              SECTION_HEADER_TABLE_ENTRY_SIZE is here-SECTION_HEADER_TABLE_FILE_OFFSET,
              assert(SECTION_HEADER_TABLE_ENTRY_SIZE == 40, 'SECTION_HEADER_TABLE_ENTRY_SIZE == 40'),

              %% .shstrtab entry
              SHSTRTAB_SECTION_HEADER_INDEX is ((here-SECTION_HEADER_TABLE_FILE_OFFSET)/SECTION_HEADER_TABLE_ENTRY_SIZE),
              SECTION_HEADER_STRING_TABLE_INDEX is SHSTRTAB_SECTION_HEADER_INDEX,

              struct([
                      sh_name=SHSTRTAB_STRING_TABLE_INDEX, % Section name (string tbl index)
                      const('SHT_STRTAB'), % sh_type, % Section type
                      sh_flags=0, % Section flags
                      sh_addr=0, % Section virtual addr at execution
                      sh_offset=STATIC_STRING_TABLE_FILE_OFFSET, % Section file offset
                      sh_size=STATIC_STRING_TABLE_SIZE, % Section size in bytes
                      sh_link=0, % Link to another section
                      sh_info=0, % Additional section information
                      sh_addralign=0, % Section alignment
                      sh_entsize=0 % Entry size if section holds table
                     ]),

              %% .symtab entry
              %% [RELOC] SYMTAB_SECTION_HEADER_INDEX is ((here-SECTION_HEADER_TABLE_FILE_OFFSET)/SECTION_HEADER_TABLE_ENTRY_SIZE),
              struct([
                      sh_name=SYMTAB_STRING_TABLE_INDEX, % Section name (string tbl index)
                      const('SHT_SYMTAB'), % sh_type, % Section type
                      sh_flags=0, % Section flags
                      sh_addr=0, % Section virtual addr at execution
                      sh_offset=SYMTAB_FILE_OFFSET, % Section file offset
                      sh_size=SYMTAB_SIZE, % Section size in bytes
                      sh_link=STRTAB_SECTION_HEADER_INDEX, % Link to another section
                      sh_info=NUMBER_OF_LOCAL_SYMBOLS,
                      sh_addralign=4, % Section alignment
                      sh_entsize=SYMTAB_ENTRY_SIZE % Entry size if section holds table
                     ]),
              %% .strtab entry
              STRTAB_SECTION_HEADER_INDEX is ((here-SECTION_HEADER_TABLE_FILE_OFFSET)/SECTION_HEADER_TABLE_ENTRY_SIZE),
              struct([
                      sh_name=STRTAB_STRING_TABLE_INDEX, % Section name (string tbl index)
                      const('SHT_STRTAB'), % sh_type, % Section type
                      sh_flags=0, % Section flags
                      sh_addr=0, % Section virtual addr at execution
                      sh_offset=STRTAB_FILE_OFFSET, % Section file offset
                      sh_size=STRTAB_SIZE, % Section size in bytes
                      sh_link=0, % Link to another section
                      sh_info=0, % Additional section information
                      sh_addralign=0, % Section alignment
                      sh_entsize=0 % Entry size if section holds table
                     ]),

              %% [RELOC]
              %% %% .rel.rodata
              %% %% RELRODATA_SECTION_HEADER_INDEX is ((here-SECTION_HEADER_TABLE_FILE_OFFSET)/SECTION_HEADER_TABLE_ENTRY_SIZE),
              %% struct([
              %%         sh_name=RELRODATA_STRING_TABLE_INDEX, % Section name (string tbl index)
              %%         const(RelocationType), % sh_type, % Section type
              %%         sh_flags=0, % Section flags
              %%         sh_addr=0, % Section virtual addr at execution
              %%         sh_offset=RELRODATA_FILE_OFFSET, % Section file offset
              %%         sh_size=RELRODATA_SIZE, % Section size in bytes
              %%         sh_link=SYMTAB_SECTION_HEADER_INDEX, % Link to another section (symtab)
              %%         sh_info=RODATA_SECTION_HEADER_INDEX, % Additional section information (section to relocate)
              %%         sh_addralign=4, % Section alignment
              %%         sh_entsize=RELRODATA_ENTRY_SIZE % Entry size if section holds table
              %%        ]),

              %% MIPS specific
              MIPS_REGINFO_SECTION_HEADER_TEMPLATE,              
              %% .rodata
              RODATA_SECTION_HEADER_INDEX is ((here-SECTION_HEADER_TABLE_FILE_OFFSET)/SECTION_HEADER_TABLE_ENTRY_SIZE),
              struct([
                      sh_name=RODATA_STRING_TABLE_INDEX, % Section name (string tbl index)
                      const('SHT_PROGBITS'), % sh_type, % Section type
                      const('SHF_ALLOC'), % sh_flags, % Section flags
                      sh_addr=0, % Section virtual addr at execution
                      sh_offset=RODATA_FILE_OFFSET, % Section file offset
                      sh_size=RODATA_SIZE, % Section size in bytes
                      sh_link=0, % Link to another section
                      sh_info=0, % Additional section information
                      sh_addralign=8, % Section alignment
                      sh_entsize=0 % Entry size if section holds table
                     ]),
              SECTION_HEADER_TABLE_ENTRY_COUNT is (here-SECTION_HEADER_TABLE_FILE_OFFSET)/SECTION_HEADER_TABLE_ENTRY_SIZE
             ]),
   DynamicSymbolTableTemplate =
       struct([
               %% undef
               SYMTAB_FILE_OFFSET is here,
               struct([
                       st_name=0, % Symbol name (string tbl index)
                       st_value=0, % Symbol value
                       st_size=0, % Symbol size
                       st_info=0, % Symbol type and binding
                       st_other=0, % No defined meaning, 0
                       st_shndx=0 % Section index
                      ]),
               SYMTAB_ENTRY_SIZE is here-SYMTAB_FILE_OFFSET,
               assert(SYMTAB_ENTRY_SIZE == 16, 'SYMTAB_ENTRY_SIZE == 16'),

               %% FILE
               struct([
                       st_name=FILE_NAME_STRING_TABLE_OFFSET, % Symbol name (string tbl index)
                       st_value=0, % Symbol value
                       st_size=0, % Symbol size
                       st_info=st_info(const('STB_LOCAL'),const('STT_FILE')), % Symbol type and binding
                       st_other=0, % No defined meaning, 0
                       const('SHN_ABS') % st_shndx=0 % Section index
                      ]),

               %% [RELOC]
               %% %% RODATA section
               %% RODATA_SECTION_SYMBOL_SYMBOL_TABLE_INDEX is (here-SYMTAB_FILE_OFFSET)/SYMTAB_ENTRY_SIZE,
               %% struct([
               %%         st_name=0, % Symbol name (string tbl index)
               %%         st_value=0, % Symbol value
               %%         st_size=0, % Symbol size
               %%         st_info=st_info(const('STB_LOCAL'),const('STT_SECTION')), % Symbol type and binding
               %%         st_other=0, % No defined meaning, 0
               %%         st_shndx=RODATA_SECTION_HEADER_INDEX % Section index
               %%        ]),
               NUMBER_OF_LOCAL_SYMBOLS is ((here-SYMTAB_FILE_OFFSET)/SYMTAB_ENTRY_SIZE),
               assert(NUMBER_OF_LOCAL_SYMBOLS == 2, 'NUMBER_OF_LOCAL_SYMBOLS == 2'),

               %% RESOURCE NAME
               %% RESOURCE_SYMBOL_SYMBOL_TABLE_INDEX is (here-SYMTAB_FILE_OFFSET)/SYMTAB_ENTRY_SIZE,
               struct([
                       st_name=RESOURCE_NAME_STRING_TABLE_OFFSET, % Symbol name (string tbl index)
                       %% [RELOC] st_value=RESOURCE_SYMBOL_SECTION_OFFSET, % Symbol value
                       %% [RELOC] st_size=RESOURCE_SYMBOL_SIZE, % Symbol size
                       st_value=RESOURCE_DATA_SECTION_OFFSET, % Symbol value (i.e., where is its data in its section)
                       st_size=RESOURCE_DATA_SIZE, % Symbol size
                       st_info=st_info(const('STB_GLOBAL'),const('STT_OBJECT')), % Symbol type and binding
                       st_other=0, % No defined meaning, 0
                       st_shndx=RODATA_SECTION_HEADER_INDEX % Section index
                      ]),
               SYMTAB_SIZE is here-SYMTAB_FILE_OFFSET
              ]),
   DynamicStringTableTemplate =
      struct([
              DYNAMIC_STRING_TABLE_FILE_OFFSET is here,
              STRTAB_FILE_OFFSET is here,
              'unsigned char'=0,
              FILE_NAME_STRING_TABLE_OFFSET is (here-DYNAMIC_STRING_TABLE_FILE_OFFSET),
              string(FILE_NAME),
              RESOURCE_NAME_STRING_TABLE_OFFSET is (here-DYNAMIC_STRING_TABLE_FILE_OFFSET),
              string(RESOURCE_NAME),
              %% DYNAMIC_STRING_TABLE_SIZE is (here-DYNAMIC_STRING_TABLE_FILE_OFFSET),
              STRTAB_SIZE is here-STRTAB_FILE_OFFSET
             ]),
      %% [RELOC]
      %% ( RelocationType == 'SHT_REL' -> % 386, ...
      %%     RELRODATA_STRING = ".rel.rodata",
      %%     REL_ENTRY = struct([
      %%                         r_offset=RESOURCE_SYMBOL_SECTION_OFFSET,
      %%                         r_info=RESOURCE_SYMBOL_RELOC_INFO,
      %%                         RESOURCE_SYMBOL_RELOC_INFO is r_info(RODATA_SECTION_SYMBOL_SYMBOL_TABLE_INDEX, const('R_ADDRESS'))
      %%                        ]),
      %%     %% Addend in section data
      %%     RESOURCE_SYMBOL_INITIAL_VALUE = ('Elf32_Word'=RESOURCE_DATA_SECTION_OFFSET),
      %%     RELRODATA_ENTRY_SIZE_SHOULD_BE = 8
      %% ; RelocationType == 'SHT_RELA' -> % SPARC, ...
      %%     RELRODATA_STRING = ".rela.rodata",
      %%     REL_ENTRY = struct([
      %%                         r_offset=RESOURCE_SYMBOL_SECTION_OFFSET,
      %%                         r_info=RESOURCE_SYMBOL_RELOC_INFO,
      %%                         RESOURCE_SYMBOL_RELOC_INFO is r_info(RODATA_SECTION_SYMBOL_SYMBOL_TABLE_INDEX, const('R_ADDRESS')),
      %%                         r_addend=RESOURCE_DATA_SECTION_OFFSET
      %%                        ]),
      %%     %% Addend in Rela entry
      %%     RESOURCE_SYMBOL_INITIAL_VALUE = ('Elf32_Word'=0),
      %%     RELRODATA_ENTRY_SIZE_SHOULD_BE = 12
      %% ),
      %%
      %% RelocSectionTemplate =
      %% struct([
      %%         RELRODATA_FILE_OFFSET is here,
      %%         REL_ENTRY,
      %%         RELRODATA_ENTRY_SIZE is here-RELRODATA_FILE_OFFSET,
      %%         assert(RELRODATA_ENTRY_SIZE==RELRODATA_ENTRY_SIZE_SHOULD_BE, 'RELRODATA_ENTRY_SIZE==RELRODATA_ENTRY_SIZE_SHOULD_BE'),
      %%         RELRODATA_SIZE is here-RELRODATA_FILE_OFFSET
      %%         ]),

   ResourceSectionTemplate =
      struct([
              RODATA_FILE_OFFSET is here,
              MIPS_REGINFO_SECTION_TEMPLATE,

              %% [RELOC]
              %% %% The memory for the resource symbol (the external variable)
              %% RESOURCE_SYMBOL_SECTION_OFFSET is here-RODATA_FILE_OFFSET,
              %% RESOURCE_SYMBOL_INITIAL_VALUE,
              %% RESOURCE_SYMBOL_SIZE is here-RESOURCE_SYMBOL_SECTION_OFFSET,

              RESOURCE_DATA_FILE_OFFSET is here,
              RESOURCE_DATA_SECTION_OFFSET is here-RODATA_FILE_OFFSET,
              %% raw_vector(ResourceData),
              ResourceDataTemplate,
              RESOURCE_DATA_SIZE is here-RESOURCE_DATA_FILE_OFFSET,
              RODATA_SIZE is here-RODATA_FILE_OFFSET
             ]),
      ( E_MACHINE == 'EM_MIPS' ->
          %% If there is no .reginfo section then the IRIX 6.5 linker
          %% (ld32) gets a segmentation fault.
          %% We generate an all zero .reginfo (as do gcc but not cc)
          MIPS_REGINFO_SECTION_TEMPLATE =
            struct([
                   REGINFO_FILE_OFFSET is here,
                   ri_gprmask=0,
                   %% ri_cprmask[4]
                   ri_cprmask=0,
                   ri_cprmask=0,
                   ri_cprmask=0,
                   ri_cprmask=0,
                   ri_gp_value=0,
                   REGINFO_SIZE is here-REGINFO_FILE_OFFSET
                   ]),
           MIPS_REGINFO_SECTION_HEADER_TEMPLATE =
              struct([
                     sh_name=REGINFO_STRING_TABLE_INDEX, % Section name (string tbl index)
                     const('SHT_MIPS_REGINFO'), % sh_type, % Section type
                     REGINFO_SH_FLAGS is const('SHF_ALLOC'),
                     sh_flags = REGINFO_SH_FLAGS,
                     sh_addr=0,     % Section virtual addr at execution
                     sh_offset=REGINFO_FILE_OFFSET, % Section file offset
                     sh_size=REGINFO_SIZE, % Section size in bytes
                     sh_link=0,     % Link to another section
                     sh_info=0,     % Additional section information
                     sh_addralign=8, % Section alignment
                     sh_entsize=0   % Entry size if section holds table
                     ])
      ; otherwise ->
          MIPS_REGINFO_SECTION_TEMPLATE = struct([]),
          MIPS_REGINFO_SECTION_HEADER_TEMPLATE = struct([])
      ),

      true.

:- barf/2 is throwing.
barf(Format, Args) :-
   format(user_error, Format, Args),
   throw(elf_error(Format, Args)).


/*
*****
***** Raw SICStus resource data (for conversion via .rsrc->rc->.res or .c->cc->.o
*****
*/

write_spres_resource_prefix(ResourceSize, ResourceMTime, Output, ResourceNameCodes) :-
   host_type(HostType),
   write_spres_resource1(size(ResourceSize), ResourceMTime, ResourceNameCodes, Output, HostType).


write_spres_resource1(ResourceInfo, ResourceMTime, ResourceNameCodes, Output, HostType) :-
   elf_default_setup(HostType, Info),
   atom_codes(Output, FileName),
   open(Output, write, Stream, [type(binary)]),
   call_cleanup(
                write_spres_resource2(ResourceInfo, ResourceMTime, FileName, ResourceNameCodes, Stream, Info),
                close(Stream)
               ).


write_spres_resource2(ResourceInfo, ResourceMTime, FileName, ResourceNameCodes, Stream, Info) :-
   link_spres_resource_file(ResourceInfo, ResourceMTime, FileName, ResourceNameCodes, FileData, Info),
   write_elf_descs(FileData, Stream, Info).


link_spres_resource_file(What, ResourceMTime, FILE_NAME, RESOURCE_NAME, FileData, Info) :-
   What = data(ResourceData),
   ResourceDataTemplate = raw_vector(ResourceData),
   resource_header_template(What, ResourceMTime, ResourceHeaderTemplate),
   link_spres_resource_file1(struct([ResourceHeaderTemplate, ResourceDataTemplate]), FILE_NAME, RESOURCE_NAME, FileData, Info).

link_spres_resource_file(What, ResourceMTime, FILE_NAME, RESOURCE_NAME, FileData, Info) :-
   What = size(ResourceSize),
   ResourceDataTemplate = size(ResourceSize),
   resource_header_template(What, ResourceMTime, ResourceHeaderTemplate),
   link_spres_resource_file1(struct([ResourceHeaderTemplate, ResourceDataTemplate]), FILE_NAME, RESOURCE_NAME, FileData, Info).


link_spres_resource_file1(ResourceDataTemplate, _FILE_NAME, _RESOURCE_NAME, FileData, Info) :-
   FileTemplate = ResourceDataTemplate,
   link_elf_description(FileTemplate, FileData, Info).


/*
*****
***** Win32 Resource (.res) file generation
*****
*/


write_win32_resource(ResourceData, ResourceName) :-
   ResourceMTime = 0,                   % Hmm
   write_win32_resource(ResourceData, ResourceMTime, ResourceName).

write_win32_resource(ResourceData, ResourceMTime, ResourceNameCodes) :-
   host_type(HostType),
   append(ResourceNameCodes, ".res", FileName),
   atom_codes(Output, FileName),
   write_win32_resource1(data(ResourceData), ResourceMTime, ResourceNameCodes, Output, HostType).


%% write_win32_resource_prefix(ResourceSize, ResourceNameCodes) :-
%%    ResourceMTime = 0,           % Hmm.
%%    host_type(HostType),
%%    append(ResourceNameCodes, ".res", FileName),
%%    atom_codes(Output, FileName),
%%    write_win32_resource_prefix(ResourceSize, ResourceMTime, Output, ResourceNameCodes).

write_win32_resource_prefix(ResourceSize, ResourceMTime, Output, ResourceNameCodes) :-
   host_type(HostType),
   write_win32_resource1(size(ResourceSize), ResourceMTime, ResourceNameCodes, Output, HostType).


write_win32_resource1(ResourceInfo, ResourceMTime, ResourceNameCodes, Output, HostType) :-
   elf_default_setup(HostType, Info),
   atom_codes(Output, FileName),
   open(Output, write, Stream, [type(binary)]),
   call_cleanup(
                write_win32_resource2(ResourceInfo, ResourceMTime, FileName, ResourceNameCodes, Stream, Info),
                close(Stream)
               ).


write_win32_resource2(ResourceInfo, ResourceMTime, FileName, ResourceNameCodes, Stream, Info) :-
   link_win32_resource_file(ResourceInfo, ResourceMTime, FileName, ResourceNameCodes, FileData, Info),
   write_elf_descs(FileData, Stream, Info).


link_win32_resource_file(What, ResourceMTime, FILE_NAME, RESOURCE_NAME, FileData, Info) :-
   What = data(ResourceData),
   ResourceDataTemplate = raw_vector(ResourceData),
   resource_header_template(What, ResourceMTime, ResourceHeaderTemplate),
   link_win32_resource_file1(struct([ResourceHeaderTemplate, ResourceDataTemplate]), FILE_NAME, RESOURCE_NAME, FileData, Info).

link_win32_resource_file(What, ResourceMTime, FILE_NAME, RESOURCE_NAME, FileData, Info) :-
   What = size(ResourceSize),
   ResourceDataTemplate = size(ResourceSize),
   resource_header_template(What, ResourceMTime, ResourceHeaderTemplate),
   link_win32_resource_file1(struct([ResourceHeaderTemplate, ResourceDataTemplate]), FILE_NAME, RESOURCE_NAME, FileData, Info).


link_win32_resource_file1(ResourceDataTemplate, FILE_NAME, RESOURCE_NAME, FileData, Info) :-
   win32_resource_file_template(ResourceDataTemplate, FILE_NAME, RESOURCE_NAME, FileTemplate, Info),
   link_elf_description(FileTemplate, FileData, Info).


win32_resource_file_template(ResourceDataTemplate, _FILE_NAME, RESOURCE_NAME0, FileTemplate, _Info) :-
   win32_encode_resource_name(RESOURCE_NAME0, RESOURCE_NAME),

   FileTemplate =
   struct([
           %% The file must start with an empty resource entry,
           %% otherwise the linker (i.e., cvtres.exe) (and other
           %% tools) will not recognize it as a resource file. I will
           %% buy a Coke to anyone who finds this documented in some
           %% official MS documentation.
           FILEHEADER_Template,
           RESOURCEHEADER_Template,
           ResourceSectionTemplate
          ]),
   %% struct RESOURCEHEADER { 
   %%     DWORD DataSize; 
   %%     DWORD HeaderSize; 
   %%     [Ordinal or name TYPE]; 
   %%     [Ordinal or name NAME]; 
   %%     DWORD DataVersion; 
   %%     WORD MemoryFlags; 
   %%     WORD LanguageId; 
   %%     DWORD Version; 
   %%     DWORD Characteristics; 
   %% };
   %% The type and name must be uppercase, otherwise FindResource(Ex)
   %% will not find it. RC.EXE does this conversion automatically.  I
   %% will buy (another) Coke to anyone who finds this documented in
   %% some official MS documentation.   
   RESOURCE_TYPE = "SICSTUSRESOURCE1.0",
   DataVersion = 0,
   MemoryFlags = 0,
   LanguageId = 0,
   Version = 0,
   Characteristics = 0,
   FILEHEADER_Template =
      struct([
              FILEHEADER_FILE_OFFFSET is here,
              'Win32_DWORD'=0,  % DataSize
              'Win32_DWORD'=FileHeaderSize, % HeaderSize
              'Win32_WORD'=0xFFFF, 'Win32_WORD'=0x0000, % TYPE
              'Win32_WORD'=0xFFFF, 'Win32_WORD'=0x0000, % NAME
              'Win32_DWORD'=0,  % DataVersion
              'Win32_WORD'=0,   % MemoryFlags
              'Win32_WORD'=0,   % LanguageId
              'Win32_DWORD'=0,  % Version
              'Win32_DWORD'=0, % Characteristics
              assert(aligned('Win32_WORD'), 'Improper alignment'), 
              FileHeaderSize is here-FILEHEADER_FILE_OFFFSET
             ]),
   RESOURCEHEADER_Template =
      struct([
              RESOURCEHEADER_FILE_OFFFSET is here,
              'Win32_DWORD'=DataSize,
              'Win32_DWORD'=HeaderSize,
              'win32_unicode_string'(RESOURCE_TYPE),
              'win32_unicode_string'(RESOURCE_NAME),
              align('Win32_DWORD'),
              assert(aligned('Win32_WORD'), 'Improper alignment'), 
              'Win32_DWORD'=DataVersion,
              'Win32_WORD'=MemoryFlags,
              'Win32_WORD'=LanguageId,
              'Win32_DWORD'=Version,
              'Win32_DWORD'=Characteristics,
              HeaderSize is here-RESOURCEHEADER_FILE_OFFFSET
             ]),
   ResourceSectionTemplate =
      struct([
              RESOURCE_DATA_FILE_OFFSET is here,
              ResourceDataTemplate,
              DataSize is here-RESOURCE_DATA_FILE_OFFSET
             ]),
   true.

%% xref fli_win.c (win32_encode_resource_name())
win32_encode_resource_name(Name, Encoding) :-
   win32_encode_charcodes(Name, Encoding).


win32_encode_charcodes([], []).
win32_encode_charcodes([Char|Chars], Encoding) :-
   %% Use HTML Numeric character references to produce an all uppercase encoding.
   html_encode_charcode(Char, Encoding,Encoding1),
   win32_encode_charcodes(Chars, Encoding1).


html_encode_charcode(Char, E,ET) :-
   E = [0'&, 0'# | E1],          % &#<DECIMALCODE>;
   ET1 = [0';|ET],
   html_encode_charcode1(Char, E1,ET1).


html_encode_charcode1(Char, E,ET) :- Char == 0, !,
   E = [0'0|ET].
html_encode_charcode1(Char, E,ET) :-
   html_encode_charcode2(Char, E,ET).

html_encode_charcode2(Char, E,ET) :- Char == 0, !,
   E = ET.
html_encode_charcode2(Char, E,ET) :-
   LSDigit is (0'0+(Char mod 10)),
   Char1 is Char//10,
   html_encode_charcode2(Char1, E, [LSDigit|ET]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

spld_file_to_cc(DataFile, ResName, OutFile) :-
   ResSize = ignore,
   file_to_cc(DataFile, ResName, ResSize, OutFile), !.
spld_file_to_cc(DataFile, ResName, OutFile) :-
   Goal=spld_file_to_cc(DataFile, ResName, OutFile),
   format(user_error, 'Goal failed: ~q~n', [Goal]),
   halt(1).


file_to_cc(DataFile, ResName, ResSize, OutFile) :-
   open(DataFile, read, In, [type(binary)]),
   call_cleanup(stream_to_cc(In, ResName, ResSize, OutFile),
                close(In)).

stream_to_cc(In, ResName, ResSize, OutFile) :-
   ( OutFile == user ->
       Out = user
   ; otherwise ->
       open(OutFile, write, Out, [type(text)])
   ),
   call_cleanup(to_cc(In, ResName, ResSize, Out),
                ( OutFile == user ->
                    true
                ; otherwise ->
                    close(Out)
                )).

to_cc(In, ResName, ResSize, Out) :-
   format(Out, '/* -*- buffer-read-only:t -*-*/~n', []),
   format(Out, '/* Automatically generated data resource, do not edit */~n~n', []),
   format(Out, '#if __cplusplus~nextern "C"~n#endif~n', []),
   %% xref flids.pl (which knows about the dimension [64])
   sp_ensure_c_name(ResName, HexName),
   format(Out, 'const char SICStus_DATA_RESOURCE_~a[][64] = { /* Data from ~a */~n', [HexName, ResName]),
   to_cc_bytes(In, Out, ActualSize),
   format(Out, '~n};~n', []),

   ( ResSize == ignore ->
       true
   ; ActualSize == ResSize ->
       true
   ; otherwise ->
       format(user_error, 'Warning: Resource size mismatch for ~a, was ~d expected ~d~n', [ResName, ActualSize, ResSize])
   ).


to_cc_bytes(In, Out, ActualSize) :-
   to_cc_bytes(In, Out, 0,ActualSize).

to_cc_bytes(In, Out, Sz0,Sz) :-
   get_byte(In, Byte),
   to_cc_bytes1(Byte, In, Out, Sz0,Sz).

to_cc_bytes1(Byte, _In, Out, Sz0,Sz) :- Byte = -1, % EOF
   !,
   Mod is Sz0 mod 64,
   ( Mod = 0 ->                 % first char
       true
   ; otherwise ->               % eof within line.
       Pad is 64-Mod,
       to_cc_pad(Pad, Out),
       put_code(Out, 0'")
   ),
   Sz=Sz0.
to_cc_bytes1(Byte, In, Out, Sz0,Sz) :-
   Mod is Sz0 mod 64,
   ( Mod = 0 ->                 % first char
       %% format(Out, '"~n   "', [])
       ( Sz0 > 0 ->
           format(Out, ',~n', [])
       ; otherwise ->
           true
       ),
       format(Out, '   "', [])
   ; otherwise ->
       true
   ),
   ( 0'0 =< Byte, Byte =< 0'9 ->
       put_code(Out, Byte)
   ; 0'a =< Byte, Byte =< 0'z ->
       put_code(Out, Byte)
   ; 0'A =< Byte, Byte =< 0'Z ->
       put_code(Out, Byte)
   ; memberchk(Byte, " ~!@#$%^&*()_+|-={}[]:;'<>./") -> % not '?', avoid tri-graph issue
       put_code(Out, Byte)
   ; otherwise ->
       to_cc_hairy(Byte, Out)
   ),
   ( Mod = 63 ->                % last char
       format(Out, '"', [])
   ; otherwise ->
       true
   ),
   Sz1 is Sz0+1,
   get_byte(In, Byte1),
   to_cc_bytes1(Byte1, In,Out, Sz1,Sz).

to_cc_hairy(Byte, Out) :-
   Digit2 is (Byte >> 6 /\ 3)+0'0,
   Digit1 is (Byte >> 3 /\ 7)+0'0,
   Digit0 is (Byte      /\ 7)+0'0,
   put_code(Out, 0'\\),
   put_code(Out,Digit2),
   put_code(Out,Digit1),
   put_code(Out,Digit0).

to_cc_pad(0, _Out) :- !.
to_cc_pad(I, Out) :-
   put_code(Out, 0'\\),
   put_code(Out, 0'0),
   I1 is I-1,
   to_cc_pad(I1, Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5

%% [PM] 3.9.1b1 This routine exists in library(resgen) AND Bips/flids.pl AND Emulator/foreign.c AND Utils/splfr.pl.in
%%              They *must* produce the same result!
%%
%% duplicated from flids.pl (cannot use prolog:sp_ensure_c_name since
%% we may run this in an older prolog)
sp_ensure_c_name(Name, HexName) :-
    atom_codes(Name, Codes),
    sp_ensure_c_name_codes(Codes, HexCodes),
    atom_codes(HexName, HexCodes).

sp_ensure_c_name_codes([], HexCodes) :-
   HexCodes = [].
sp_ensure_c_name_codes([C|Codes], HexCodes) :-
   sp_ensure_c_name_code(C, HexCodes,HexCodes1),
   sp_ensure_c_name_codes(Codes,HexCodes1).

sp_ensure_c_name_code(C, HexCodes,HexCodes1) :-
   memberchk(C, "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"), % [0-9a-zA-Z] (note that 'w' is in a-z in ASCII)
   !,
   HexCodes = [C|HexCodes1].
sp_ensure_c_name_code(C, [0'_, 0'0, 0'x, MSD, LSD|HexCodes],HexCodes) :- % "_0x"
   HiNibble is (C >> 4) /\ 0xF,
   LoNibble is C /\ 0xF,
   HexDigits = "0123456789ABCDEF",
   nth0(HiNibble, HexDigits, MSD),
   nth0(LoNibble, HexDigits, LSD).


%% [PM] 4.0.5 Runtime License

spld_resgen_prepare_license(File, Kind, LicenseFile) :-
   prepare_license(File, Kind, LicenseFile), !.
spld_resgen_prepare_license(File, Kind, LicenseFile) :-
   Goal=spld_resgen_prepare_license(File, Kind, LicenseFile),
   format(user_error, 'Goal failed: ~q~n', [Goal]),
   halt(1).

license_prefix(0, 'runtime_').                  % LICENSE_RUNTIME_PRODUCT
license_prefix(1, '').
license_prefix(2, 'extended_runtime_').         % LICENSE_EXTENDED_RUNTIME_PRODUCT

license_product(Kind, SpVer) :-
        prolog:'$current_version'(BaseProduct),
        license_prefix(Kind, Prefix),
        atom_concat(Prefix, BaseProduct, SpVer).

:- dynamic license_fact_/1.
:- volatile license_fact_/1.
extract_license_info(S, Kind, Site, License) :-
        retractall(license_fact_(_)),
        license_product(Kind, SpVer),
        repeat,
        ( license_fact_(product(_,_,_)), license_fact_(site(_)) -> % done
           !
        ; otherwise ->
           read(S, Elt),
           ( Elt == end_of_file ->
              !
           ; otherwise ->
              ground(Elt),
              ( Elt = product(SpVer,_,_) -> true ; Elt = site(_) ),
              assert(license_fact_(Elt)),
              fail
           )
        ),
        once(license_fact_(site(Site1))),
        License1 = product(_,_,_),
        once(license_fact_(License1)),
        !,
        retractall(license_fact_(_)),
        Site = Site1,
        License = License1.


prepare_license(File, Kind, LicenseFile) :- LicenseFile == '', !,  % use loaded license
        license_product(Kind, SpVer),
        prolog:license_info(SpVer,Site,License),
        emit_license(File, Site, License).

prepare_license(File, Kind, LicenseFile) :-
        open(LicenseFile, read, S),
        call_cleanup(extract_license_info(S, Kind, Site, License), close(S)),
        !,
        emit_license(File, Site, License).


emit_license(File, Site, License) :-
        open(File, write, S),
        format(S, '%% Runtime generated using the following licence:~n~k.~n~k.~n', [site(Site), License]),
        close(S).
