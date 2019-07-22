// COM Client.cpp : Defines the entry point for the console application.
//
#define SICSTUS_HIDDEN_API 1
#undef DBG /* [PM] 4.2.1 objbase.h uses DBG but nowadays we set it to a poisoned value on command line. */

#ifndef COMCLIENT_DEBUG
#define COMCLIENT_DEBUG 0
#endif

#ifndef COMCLIENT_ENABLE_BREAKPOINTS
#define COMCLIENT_ENABLE_BREAKPOINTS 0
#endif

#ifndef NO_RESETS
#define NO_RESETS 0
#endif

#if NO_RESETS
#undef SP_reset_term_refs
#define SP_reset_term_refs(X) do{(void)(X);}while(0)
#endif

/*
M-x compile
set Path=%Path%;D:\Program Files\SICStus Prolog\bin&& cw_bash -c make

(visit-tags-table "d:\\Program Files\\Microsoft Platform SDK\\Include\\TAGS")

cl /nologo /I"D:/Program Files/SICStus Prolog/include" /MLd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /Fp"Debug/COM Client.pch"  /Fo"Debug/" /Fd"Debug/" /FD /GZ /c "comclient.c"

splfr comclient.c comclient.pl

 */

// Must be first in file
// #include "stdafx.h"
/*

PL: atom/code-list <-> BSTR 
    Use MultiByteToWideChar (CP_UTF8 on NT, CP_ACP on 95/98)

   int SPGetBSTR(SP_term_ref t, BSTR *pBSTR)
   int SPPutBSTR(SP_term_ref t, BSTR bstr, long atom)
      int SysAllocSPTmpString(BSTR bstr, char **ps)
      int SysAllocStringSP(char *s, BSTR *pBSTR)

CLSIDFromProgID BSTR -> CLSID
   int comclient_CLSIDFromProgID(SP_term_ref tProgID, SP_term_ref tCLSID, long atom)
      HRESULT SP_CLSIDStringFromProgID(BSTR progID, BSTR *pCLSIDString)
   int comclient_ProgIDFromCLSID(SP_term_ref tCLSID, SP_term_ref tProgID, long atom)
      HRESULT SP_ProgIDFromCLSIDString(BSTR CLSIDString, BSTR *pProgIDString)

CLSIDFrom(To)String BSTR <-> CLSID (same as GUIDFromString?)
IIDFrom(To)String BSTR <-> IID (same as GUIDFromString?)

GUIDFrom(To)String GUID <-> BSTR



INameToIIDString BSTR -> IIDString
   [I have not found a builtin for this, now uses
   HKEY_LOCAL_MACHINE\SOFTWARE\Classes\Interface backwards]
   HRESULT INameToIID (char *iname, IID *pIID)
   HRESULT IIDToName (const IID *pIID, char **iname)
   int comclient_INameToIID(SP_term_ref tName, SP_term_ref tIID, long atom)
   int comclient_IIDToName(SP_term_ref tIID, SP_term_ref tName, long atom)


Co(Un)Initialize()


CoCreateInstance(CLSID, (CLXCTX_ANY), IID) -> IUnknown*
   HRESULT SPCreateInstance(const CLSID *rclsid, const IID *pIID, IUnknown **ppItf)
   comclient_create_instance(SP_term_ref tCLSID, SP_term_ref tIID, SP_term_ref tItf)

QueryInterface (IUnknown*, IID) -> Interface*

Release (IUnknown*)  ?
AddRef (IUnknown*)   ?

GetIDsOfName(s)(IDispatch*, BSTR) -> dispid

Invoke(IDispatch*, dispid, DISPATCH_METHOD/Put/Get/PutRef/???, PARAMS) ->
Result, err/exception info

GetTypeInfo(IDispatch*) -> ITypeInfo*

LoadRegTypeLib (GUID, major, minor) -> ITypeLib* ??

ITypeInfo & ITypeLib routines

Building a DISPPARAMS structure with and without type and direction
info

Calling a method(/function) given a Type Library.



Arg type description:
Direction:
   in
      VARIANT: obvious
   out
      VARIANT: VT_BYREF pointing to valid "zero"-initialized storage.
      OTHER: Pointer to storage as above
   both?
      As out but storage inited as for in
   retval?
      ??

Type:
   VT_XXX
   VT_BYREF / VT_PTR? for void* (not valid in VARIANT)

Default translation:
   integer -> in VT_int/long/I8/???
   float   -> in VT_float/double??
   atom    -> in BSTR
   list    -> in BSTR
   Var     -> out VT_VARIANT  (i.e., | VT_BYREF) (initialized as VT_EMPTY/INVALID?)

*/
#define _WIN32_DCOM 1

#define CINTERFACE 1
#define COBJMACROS 1 // OAIdl.h defines IDispatch_AddRef(This, ...) etc.
#include <stdio.h>
#include <windows.h>
#include <tchar.h>

#include <sicstus/sicstus.h>
#include "comclient_glue.h"


#include <OAIdl.h>
// [PM] 3.10 not in VS .NEt 2003 BETA nor WinCE
// #include <iostream.h>
#include <objbase.h> // CoInitialize(Ex)
#include <wtypes.h>

/* [PM] 4.1.3 These should all be in comclient_glue.h so no need to declare here */
#if 0

// Exported functions
// comclient_create_instance(+CLSID:term, +IID:term, -ITF:term).
// CLSID and IID are GUIDs in textual form (e.g., from comclient_CLSIDStringFromProgID
extern void SPCDECL comclient_create_instance(SP_term_ref tCLSID, SP_term_ref tIID, SP_term_ref tItf, SP_integer *result);

// comclient_CLSIDFromProgID(+ProgID:term, -CLSID:term, +atom?:int)
extern void SPCDECL comclient_CLSIDFromProgID(SP_term_ref tProgID, SP_term_ref tCLSID, SP_integer atom, SP_integer *result);
extern void SPCDECL comclient_ProgIDFromCLSID(SP_term_ref tCLSID, SP_term_ref tProgID, SP_integer atom, SP_integer *result);

// comclient_INameToIID(+Name:term, -IID:term, +atom?:int)
extern void SPCDECL comclient_INameToIID(SP_term_ref tName, SP_term_ref tIID, SP_integer atom, SP_integer *result);
// comclient_IIDToName(+IID:term, -Name:term, +atom?:int, -result:int)
extern void SPCDECL comclient_IIDToName(SP_term_ref tIID, SP_term_ref tName, SP_integer atom, SP_integer *result);

// comclient_invoke(+Obj:term, +Goal:term, +Flags:int, -Result:term, -HResult:integer)
extern void SPCDECL comclient_invoke(SP_term_ref tObj, SP_term_ref tGoal, SP_term_ref tFlags, SP_term_ref tAnswer, SP_integer *result);
extern void SPCDECL comclient_invoke_ex(SP_term_ref tObj, SP_term_ref tGoal, SP_term_ref tFlags, SP_term_ref tAnswer, SP_term_ref tResult, SP_integer *result);

extern void SPCDECL comclient_init(int when);
extern void SPCDECL comclient_deinit(int when);

#endif  /* 0 */

#if COMCLIENT_DEBUG
static comclient_enable_breakpoints = COMCLIENT_ENABLE_BREAKPOINTS;
static char *atom_comclient_error_name = "foo";

#define BreakPoint()        do { if (comclient_enable_breakpoints) _asm { int 3h }; } while(0)
#define EntryBreakPoint(NAME) BreakPoint()
#else
#define BreakPoint() do{}while(0)
#define EntryBreakPoint(NAME) BreakPoint()
#endif

// EXPORTED
SP_integer SPCDECL comclient_breakpoints_flag(SP_integer on)
{
#if COMCLIENT_DEBUG
  int oldvalue = comclient_enable_breakpoints;
  comclient_enable_breakpoints = (on ? 1 : 0);
  return oldvalue;
#else
  return 0;
#endif
}


static SP_atom atom_comclient_error;
static SP_atom atom_comclient_object;


static LCID defaultLCID = LOCALE_SYSTEM_DEFAULT;

static int coinitialize_called = 0;
static SP_external_object_type external_object_type__interface = NULL;


// Allocate a BSTR from a SICStus atomcoded string.
// Returns 0 on failure.
// Does not free any preexisting BSTR in *pBSTR
// Attempts to translate using UTF8 but this only works on NT so it
// falls back on CP_ACP.
static int SysAllocStringSP(char const *s, BSTR *pBSTR)
{
  BSTR bstr = NULL;
  int len1, len1x;
  UINT CodePage;
  
  if (s[0] == '\0')
    {
      pBSTR = NULL;
      return 1;
    }

  // Find the length and a usable CodePage
  CodePage = CP_UTF8;      // NT Only
  while (1)
    {
      len1 = 
        MultiByteToWideChar(
                            CodePage, // UINT CodePage,     // code page
                            0,    // DWORD dwFlags,         // character-type options
                            s,    // LPCSTR lpMultiByteStr, // string to map
                            -1,   // int cbMultiByte,       // number of bytes in string
                            NULL, // LPWSTR lpWideCharStr,  // wide-character buffer
                            0    // int cchWideChar        // size of buffer
                            );
      if (len1 > 0 || CodePage == CP_ACP) break;
      CodePage = CP_ACP;        // assume failure due to non-NT and CP_UTF8
    }
  if (! (len1 > 0) ) return 0;  // failed
  if (! (len1 > 1) ) return 0;  // cannot happen

  // len1 includes the terminating NULL char

#if 1
  /* [PM] 4.2.1+ pointless (and invalid) to reallocate bstr since it is NULL here */
  bstr = SysAllocStringLen(NULL, (unsigned int) len1-1);
  if (bstr == NULL) {
    return 0;
  }
#else /* !1 */
  if (! SysReAllocStringLen( &bstr, NULL, (unsigned int) len1-1) ) return 0;
#endif /* !1 */

  len1x =
    MultiByteToWideChar(
                        CodePage, // UINT CodePage,     // code page
                        0,    // DWORD dwFlags,         // character-type options
                        s,    // LPCSTR lpMultiByteStr, // string to map
                        -1,   // int cbMultiByte,       // number of bytes in string
                        bstr, // LPWSTR lpWideCharStr,  // wide-character buffer
                        len1 // int cchWideChar        // size of buffer
                        );
  // should always be equal unless len1x is 0 due to out of memory
  if (len1x != len1)
    {
      SysFreeString(bstr);
      return 0;
    }
  *pBSTR = bstr;
  return 1;
}

static char *FreeTmpString(char *s)
{
  CoTaskMemFree((BSTR) s);
  return NULL;
}

static char *AllocTmpStringSize(size_t size)
{
  return (char *) CoTaskMemAlloc(size);
}

static char *AllocTmpString(char *s)
{
  size_t len = strlen(s);
  char *s1 = AllocTmpStringSize(len+1);

  if (s1)
    {
      strcpy(s1, s);
    }
  return s1;  
}

static int SysAllocSPTmpString(BSTR bstr, char **ps)
{
  int len;
  int size;			// Note: intentionally int, not size_t
  UINT CodePage;

  SP_ASSERT(bstr != NULL); /* [PM] 4.2.3 suppress PreFast warning */

  len = SysStringLen(bstr);

  // Find the length and a usable CodePage
  CodePage = CP_UTF8;      // NT Only
  while (1)
    {
      size = 
        WideCharToMultiByte(
                            CodePage, // UINT CodePage,            // code page
                            0,     // DWORD dwFlags,            // performance and mapping flags
                            bstr,  // LPCWSTR lpWideCharStr,    // wide-character string
                            // len+1 to allow embedded NUL chars
                            -1, // int cchWideChar,          // number of chars in string
                            NULL,  // LPSTR lpMultiByteStr,     // buffer for new string
                            0,     //int cbMultiByte,          // size of buffer
                            NULL,  // LPCSTR lpDefaultChar,     // default for unmappable chars
                            NULL   // LPBOOL lpUsedDefaultChar  // set when default char used
                            );
      if (size > 0 || CodePage == CP_ACP) break;
      CodePage = CP_ACP;        // assume failure due to non-NT and CP_UTF8
    }
  if (! (size > 0) ) return 0;  // failed
  if (! (size > 1) ) return 0;  // cannot happen

  {
    char *s = AllocTmpStringSize(size);

    if (!s)
      {
        return 0;               // failed
      }
    else
      {
        int size1 = // Note: intentionally int, not size_t
          WideCharToMultiByte(
                              CodePage, // UINT CodePage,            // code page
                              0,     // DWORD dwFlags,            // performance and mapping flags
                              bstr,  // LPCWSTR lpWideCharStr,    // wide-character string
                              // len+1 to allow embedded NUL chars
                              -1, // int cchWideChar,          // number of chars in string
                              s,  // LPSTR lpMultiByteStr,     // buffer for new string
                              size,     //int cbMultiByte,          // size of buffer
                              NULL,  // LPCSTR lpDefaultChar,     // default for unmappable chars
                              NULL   // LPBOOL lpUsedDefaultChar  // set when default char used
                              );
        if (size1 != size)      // cannot happen
          {
            FreeTmpString(s);
            return 0;
          }

        *ps = s;
        return 1;               // success
      }
  }
}


// Allocates a BSTR initialized from an atom
// returns 0 on failure
// pBSTR is assumed to point at uninitialized memory, ie, previous
// BSTR is not freed
static int SPGetBSTR(SP_term_ref tr, BSTR *pBSTR)
{ 
  char *s = NULL;

  if ( SP_is_atom(tr)
       && SP_get_string(tr, &s) )
    {
      if (s[0] == '[' && s[1] == ']' && s[2] == '\0') // [], i.e., ""
        {
          *pBSTR = NULL; // i.e., an empty BSTR
          return 1;
        }
      else
        {
          return SysAllocStringSP(s, pBSTR);
        }
    }
  else
    {
      int i;
      int rc = 0;
      BSTR bstr = NULL;
      SP_integer len = 0;
      SP_term_ref tmp, car, cdr;
      
      tmp = SP_new_term_ref();
      car = SP_new_term_ref();
      cdr = SP_new_term_ref();
      
      for ( (void)SP_put_term(tmp, tr); SP_get_list(tmp, car, cdr); tmp = cdr, len++)
        {
          SP_integer c;
          
          if (!SP_get_integer(car, &c)) goto cleanup;
          if (! (0 < c && c <= 0xFFFF )) goto cleanup;
        }

      // Ignore the issue of non-proper lists
      // if (!SP_get_nil(tr)) goto cleanup;

      {
	UINT ui = (UINT) len;

	if (((SP_integer) ui) != len)
	{
	  /* [PM] 4.2.1+ len > 4GB, not likely */
	  SP_SOFT_ASSERT(0);
	  rc = 0; /* failure */
	  goto cleanup;
	}

	bstr = SysAllocStringLen(NULL, ui);
      }

      if (!bstr) goto cleanup;

      for (i = 0, tmp = tr; SP_get_list(tmp, car, cdr); tmp = cdr, i++)
        {
          SP_integer c;
          
          if (!SP_get_integer(car, &c)) goto cleanup;
          // ASSERT (0 < c && c <= 0xFFFF )
          // ASSERT (i <= len)
          bstr[i] = (OLECHAR) c;
        }
      *pBSTR = bstr;
      bstr = NULL; // prevent free in cleanup;
      rc = 1;                   // success
    cleanup:
      SP_reset_term_refs(car);
      if (bstr)                 // NULL also if passed out through pBSTR
        {
          SysFreeString(bstr);
        }
      return rc;
    }
}

// Allocate a prolog term representing BSTR
// if atom is true then an atom is created, otherwise a UNICODE char
// code list.
// The term ref should already be allocated.
// Does *not* free the BSTR
static int SPPutBSTR(SP_term_ref t, BSTR bstr, SP_integer atom)
{
  int rc = 1;
  UINT len = SysStringLen(bstr);
  if (atom)
    {
      if (! (len > 0) )
        {
          (void) SP_put_string(t, "");
        }
      else
        {
          char *s = NULL;
          
          if (!SysAllocSPTmpString(bstr, &s))
            {
              return 0;
            }
          
          rc = SP_put_string(t, s);
          
          FreeTmpString(s);
        }
    }
  else                          // make list
    {
      if (!SP_put_string(t, "[]"))
        {
          return 0;
        }
      
      if (! (len > 0) )
        {
          /* empty */;
        }
      else
        {
          UINT i;
          SP_term_ref tmp = SP_new_term_ref();

          for (i = len; i;)
            {
              if (!SP_put_integer(tmp, (SP_integer) bstr[--i]))
                {
                  rc = 0;
                  break;
                }

              if (!SP_cons_list(t, tmp, t))
                {
                  rc = 0;
                  break;
                }
            }
          SP_reset_term_refs(tmp);
        }
    }
  return rc;
}

// Allocate a BSTR representation of the CLSID corresponding to
// progID.
// *pCLSIDString is not deallocated.
static HRESULT SP_CLSIDStringFromProgID(BSTR progID, BSTR *pCLSIDString)
{
  HRESULT rc = S_OK;
  CLSID clsid;
  LPOLESTR olestr = NULL;
  BSTR clsidBSTR = NULL;
  
  rc = CLSIDFromProgID(progID, &clsid);
  if (!SUCCEEDED(rc)) goto cleanup;
  
  rc = StringFromCLSID(&clsid, &olestr);
  if (!SUCCEEDED(rc)) goto cleanup;

  clsidBSTR = SysAllocString(olestr);
  if (!clsidBSTR)
    {
      rc = E_OUTOFMEMORY;
      goto cleanup;
    }
      
  *pCLSIDString = clsidBSTR;

 cleanup:
  if (olestr) CoTaskMemFree(olestr);
  return rc;
}

static HRESULT SP_ProgIDFromCLSIDString(BSTR CLSIDString, BSTR *pProgIDString)
{
#if !WINCE
  HRESULT rc = S_OK;
  CLSID clsid;
  LPOLESTR olestr = NULL;
  BSTR progidBSTR = NULL;

  // This, apparently, goes to the registry to verify validity
  rc = CLSIDFromString(CLSIDString, &clsid);
  if (!SUCCEEDED(rc)) goto cleanup;
  
  rc = ProgIDFromCLSID(&clsid, &olestr);
  if (!SUCCEEDED(rc)) goto cleanup;
  
  progidBSTR = SysAllocString(olestr);
  if (!progidBSTR)
    {
      rc = E_OUTOFMEMORY;
      goto cleanup;
    }
      
  *pProgIDString = progidBSTR;

 cleanup:
  if (olestr) CoTaskMemFree(olestr);
  return rc;
#else  /* WINCE */
#error "Check the article \"OPC for Windows CE\" for some ideas/PM"
#error "Pocket PC 2003 has COM?"
  return S_FALSE;
#endif  /* WINCE */
}


// Obtain the CLSID from a ProgID
// Put the string representation of a the CLSID into tCLSID (as a code 
// list or atom depending on the atom arg).
void SPCDECL comclient_CLSIDFromProgID(SP_term_ref tProgID, SP_term_ref tCLSID, SP_integer atom, SP_integer *result)
{
  HRESULT hr = E_FAIL;
  BSTR progID = NULL;
  BSTR clsidBSTR = NULL;

  EntryBreakPoint("comclient_CLSIDFromProgID");

  if (!SPGetBSTR(tProgID, &progID)) goto cleanup;

  hr = SP_CLSIDStringFromProgID(progID, &clsidBSTR);
  if (!SUCCEEDED(hr)) goto cleanup;

  if (!SPPutBSTR(tCLSID, clsidBSTR, atom))
    {
      hr = DISP_E_TYPEMISMATCH; // Hmm
      goto cleanup;
    }

 cleanup:
  SysFreeString(progID);
  SysFreeString(clsidBSTR);
  *result = hr;
}

// Obtain the ProgID from a CLSID
// Put the string representation of a the ProgID into tProgID (as a code 
// list or atom depending on the atom arg).
void SPCDECL comclient_ProgIDFromCLSID(SP_term_ref tCLSID, SP_term_ref tProgID, SP_integer atom, SP_integer *result)
{
  HRESULT hr;
  BSTR progID = NULL;
  BSTR clsidBSTR = NULL;

  EntryBreakPoint("comclient_ProgIDFromCLSID");

  if (!SPGetBSTR(tCLSID, &clsidBSTR))
    {
      hr = DISP_E_TYPEMISMATCH; // Hmm
      goto cleanup;
    }

  hr = SP_ProgIDFromCLSIDString(clsidBSTR, &progID);
  if (!SUCCEEDED(hr)) goto cleanup;

  if (!SPPutBSTR(tProgID, progID, atom))
    {
      hr = DISP_E_TYPEMISMATCH; // Hmm
      goto cleanup;
    }

 cleanup:
  SysFreeString(progID);
  SysFreeString(clsidBSTR);
  *result = hr;
}

// find the IID of an interface given its name as a char string
// Uses the registry.
// In the future this could instead/also use type libraries
static HRESULT INameToIID (char *iname, IID *pIID)
{
#if WINCE
  return S_FALSE; // No interface directory on CE? [CN] FIXME:
#else  /* !WINCE */
  HRESULT rc = S_OK;
  HKEY hKey = HKEY_LOCAL_MACHINE;
  DWORD dwIndex;
  int done;

  rc = RegOpenKeyEx(
                    HKEY_LOCAL_MACHINE, // HKEY hKey,         // handle to open key
                    _T("SOFTWARE\\Classes\\Interface"), // LPCTSTR lpSubKey,  // subkey name
                    0, // DWORD ulOptions,   // reserved
                    KEY_READ, // REGSAM samDesired, // security access mask
                    &hKey // PHKEY phkResult    // handle to open key
                    );
  if (!SUCCEEDED(rc)) goto cleanup;
  
  for (done = 0, dwIndex = 0; !done && SUCCEEDED(rc); dwIndex++)
    {
      char iidstring[MAX_PATH+1];
      DWORD bufsize = (sizeof iidstring)/(sizeof iidstring[0]);
      // Using RegEnumKeyEx since RegEnumKey is missing from WinCE
      rc = RegEnumKeyExA(
                      hKey, // HKEY hKey,     // handle to key to query
                      dwIndex, // DWORD dwIndex, // index of subkey to query
                      iidstring, // LPTSTR lpName, // buffer for subkey name
                      &bufsize, // DWORD cbName   // size of subkey name buffer
		      NULL, NULL, NULL, NULL
                      );
      // Have now found HKLM\Classes\Interface\{IID} open it to see if
      // its value is 'iname'
      if (SUCCEEDED(rc))
        { // this block should use rc1, never rc
          HRESULT rc1;
          HKEY hKey1;

          rc1 = RegOpenKeyExA(
                             hKey, // HKEY hKey,         // handle to open key
                             iidstring, // LPCTSTR lpSubKey,  // subkey name
                             0, // DWORD ulOptions,   // reserved
                             KEY_READ, // REGSAM samDesired, // security access mask
                             &hKey1 // PHKEY phkResult    // handle to open key
                             );
          if (SUCCEEDED(rc1))
            {
              char iname1[MAX_PATH+1];
              DWORD type;
              DWORD cbData = (sizeof iname1)/(sizeof *iname1);

              rc1 = RegQueryValueEx(
                                    hKey1, // HKEY hKey,            // handle to key
                                    NULL, // LPCTSTR lpValueName,  // value name
                                    NULL, // LPDWORD lpReserved,   // reserved
                                    &type, // LPDWORD lpType,       // type buffer
                                    iname1, // LPBYTE lpData,        // data buffer
                                    &cbData  // LPDWORD lpcbData      // size of data buffer
                                    );

              if (SUCCEEDED(rc1))
                {
                  (void) RegCloseKey(hKey1);
                }
              if (SUCCEEDED(rc1) && type == REG_SZ)
                {
                  // ??? Are interface names case sensitive or not?
                  // In either case, should use really use UNICODE on NT
                  if (!strcmp(iname1, iname))
                    {
                      BSTR iidBSTR = NULL;
                      // iidstring is the char IIDstring of interface 'iname'
                      if (SysAllocStringSP(iidstring, &iidBSTR))
                        {
                          IID iid;

                          rc1 = IIDFromString(iidBSTR, &iid);
                          SysFreeString(iidBSTR);

                          if (SUCCEEDED(rc1))
                            {
                              *pIID = iid;
                              done = 1;
                            }
                        }
                    }
                }
            }
        }
    }
  
  // invariant done -> SUCCEEDED(rc)

 cleanup:
  if (hKey != HKEY_LOCAL_MACHINE)
    {
      (void) RegCloseKey(hKey);
    }

  return rc;
#endif  /* !WINCE */
}

static HRESULT INameToIIDU (BSTR name, IID *pIID)
{
  char *iname = NULL;
  HRESULT hr;

  if (!SysAllocSPTmpString(name, &iname)) return DISP_E_TYPEMISMATCH; // Hmm
  
  hr = INameToIID(iname, pIID);
  FreeTmpString(iname);

  return hr;
}

// Find the name (a char string) of an interface
// Deallocate the name with FreeTmpString
// Uses the registry
// In the future this could instead/also use type libraries
static HRESULT IIDToName (const IID *pIID, char **iname)
{
#if WINCE
  return S_FALSE;
#else  /* !WINCE */
  HRESULT hr = S_OK;
  HKEY hKey = HKEY_LOCAL_MACHINE;
  DWORD dwIndex;
  int done;

  hr = RegOpenKeyEx(
                    HKEY_LOCAL_MACHINE, // HKEY hKey,         // handle to open key
                    _T("SOFTWARE\\Classes\\Interface"), // LPCTSTR lpSubKey,  // subkey name
                    0, // DWORD ulOptions,   // reserved
                    KEY_READ, // REGSAM samDesired, // security access mask
                    &hKey // PHKEY phkResult    // handle to open key
                    );
  
  for (done = 0, dwIndex = 0; !done && SUCCEEDED(hr); dwIndex++)
    {
      char iidstring[MAX_PATH+1];
      hr = RegEnumKeyA(
                      hKey, // HKEY hKey,     // handle to key to query
                      dwIndex, // DWORD dwIndex, // index of subkey to query
                      iidstring, // LPTSTR lpName, // buffer for subkey name
                      (sizeof iidstring)/(sizeof *iidstring) // DWORD cbName   // size of subkey name buffer
                      );
      // Have now found HKLM\Classes\Interface\{IID}
      // See if {IID} is string form of *pIID
      if (SUCCEEDED(hr))
        {
          BSTR iidBSTR = NULL;

          if (SysAllocStringSP(iidstring, &iidBSTR))
            {
              IID iid;
              HRESULT hr1;

              hr1 = IIDFromString(iidBSTR, &iid);
              SysFreeString(iidBSTR);
              if ( SUCCEEDED(hr1)
                   && IsEqualIID(&iid, pIID) )
                {
                  char iname1[MAX_PATH+1];
                  DWORD type;
                  DWORD cbData = (sizeof iname1)/(sizeof *iname1);
                  HKEY hKey1;

                  done = 1;
                  
                  hr = RegOpenKeyExA(
                                    hKey, // HKEY hKey,         // handle to open key
                                    iidstring, // LPCTSTR lpSubKey,  // subkey name
                                    0, // DWORD ulOptions,   // reserved
                                    KEY_READ, // REGSAM samDesired, // security access mask
                                    &hKey1 // PHKEY phkResult    // handle to open key
                                    );
                  if (SUCCEEDED(hr))
                    {
                      hr = RegQueryValueEx(
                                           hKey1, // HKEY hKey,            // handle to key
                                           NULL, // LPCTSTR lpValueName,  // value name
                                           NULL, // LPDWORD lpReserved,   // reserved
                                           &type, // LPDWORD lpType,       // type buffer
                                           iname1, // LPBYTE lpData,        // data buffer
                                           &cbData  // LPDWORD lpcbData      // size of data buffer
                                           );
                      (void) RegCloseKey(hKey1);
                    }

                  if (SUCCEEDED(hr) && type == REG_SZ)
                    {
                      char *name;
                      name = AllocTmpString(iname1);
                      if (name)
                        {
                          *iname = name;
                        }
                      else
                        {
                          hr = E_OUTOFMEMORY;
                        }
                    }
                }
            }
          else
            {
              hr = E_OUTOFMEMORY;
            }
        }
    }
  // invariant done -> SUCCEEDED(hr)

  // cleanup:
  if (hKey != HKEY_LOCAL_MACHINE)
    {
      (void) RegCloseKey(hKey);
    }

  return hr;
#endif  /* !WINCE */
}

#if 0
comclient_raise_exception(char *message)
{
  SP_term_ref tr;
  tr = SP_new_term_ref();

  SP_put_string(tr, message);
  SP_cons_functor(tr, atom_comclient_error, 1, tr);
  SP_raise_exception(tr);
}
#endif

// comclient_INameToIID(+Name, -IID, +integer, -integer)
void SPCDECL comclient_INameToIID(SP_term_ref tName, SP_term_ref tIID, SP_integer atom, SP_integer *result)
{
  LPOLESTR olestr = NULL;
  BSTR bstrIID = NULL;
  char *name;
  IID iid;
  HRESULT hr = E_FAIL;

  EntryBreakPoint("comclient_INameToIID");

  if (!SP_get_string(tName, &name)) goto cleanup;

  hr = INameToIID(name, &iid);
  if (!SUCCEEDED(hr)) goto cleanup;

  hr = StringFromIID(&iid, &olestr);
  if (!SUCCEEDED(hr)) goto cleanup;
  
  bstrIID = SysAllocString(olestr);
  if (!bstrIID)
    {
      hr = E_OUTOFMEMORY;
      goto cleanup;
    }

  if (!SPPutBSTR(tIID, bstrIID, atom))
    {
      hr = DISP_E_TYPEMISMATCH; // Hmm
      goto cleanup;
    }

 cleanup:
  SysFreeString(bstrIID);
  if (olestr) CoTaskMemFree(olestr);
  // if (spErr != SP_SUCCESS) SP_raise_exception("comclient_INameToIID");
  *result = hr;
}

// comclient_iid_name(+IID, -Name, +integer)
void SPCDECL comclient_IIDToName(SP_term_ref tIID, SP_term_ref tName, SP_integer atom, SP_integer *result)
{
#if !WINCE
  BSTR bstrIID = NULL;
  char *szIID;
  IID iid;
  HRESULT hr;
  SP_term_ref tail;
    
  tail = SP_new_term_ref();   /* [] */

  EntryBreakPoint("comclient_IIDToName");

  if (!SPGetBSTR(tIID, &bstrIID))
    {
      hr = DISP_E_TYPEMISMATCH; // Hmm
      goto cleanup;
    }

  hr = IIDFromString(bstrIID, &iid);
  if (!SUCCEEDED(hr)) goto cleanup;

  hr = IIDToName(&iid, &szIID);
  if (!SUCCEEDED(hr)) goto cleanup;

  if (( atom ? SP_put_string(tName, szIID) : SP_put_list_codes(tName, tail, szIID)))
    {
      hr = DISP_E_TYPEMISMATCH; // Hmm
      goto cleanup;
    }

 cleanup:
  SP_reset_term_refs(tail);

  SysFreeString(bstrIID);
  // if (spErr != SP_SUCCESS) SP_raise_exception("comclient_IIDToName");
  *result = hr;
#else  /* WINCE */
  *result = S_FALSE;
#endif  /* WINCE */
}

#if 0
typedef int (*map_registry_fun)(HKEY hKey, char *subKeyName, void *userData);

static HRESULT map_registry (HKEY hKeyStart,
                             const char *subKeyA,
                             map_registry_fun fun,
                             void *userData,
                             REGSAM samDesired) // at least KEY_READ
{
  HRESULT hr = S_OK;
  HKEY hKey = HKEY_LOCAL_MACHINE;
  DWORD dwIndex;
  int done;
  TCHAR const *subKeyT;
#ifdef _UNICODE
  WCHAR* subKeyW;
  subKeyW = malloc((strlen(subKeyA) + 1) * sizeof subKeyW[0]);
  if (!subKeyW)
    {
#if SICSTUS_TODO
#error "[PM] error handling"
#error "[PM] 4.0 FIXME: Use Unicode Win API"
#error "[PM] 4.0 registry-free Com (manifests etc). .NET?"
#endif  /* SICSTUS_TODO */
    }
  mbstowcs(subKeyW, subKeyA, (strlen(subKeyA) + 1));
  subKeyT = subKeyW;
#else  /* !_UNICODE */
  subKeyT = subKeyA;
#endif  /* !_UNICODE */

  hr = RegOpenKeyEx(
                    hKeyStart, // HKEY hKey,         // handle to open key
                    subKeyT, // LPCTSTR lpSubKey,  // subkey name
                    0, // DWORD ulOptions,   // reserved
                    samDesired, // REGSAM samDesired, // security access mask
                    &hKey // PHKEY phkResult    // handle to open key
                    );
  if (!SUCCEEDED(hr)) goto cleanup;
  
  for (done = 0, dwIndex = 0; !done && SUCCEEDED(hr); dwIndex++)
    {
      TCHAR iidstring[MAX_PATH+1];
      DWORD iidstring_buflen = ((sizeof iidstring)/(sizeof iidstring[0]));

      hr = RegEnumKeyEx(
                      hKey, // HKEY hKey,     // handle to key to query
                      dwIndex, // DWORD dwIndex, // index of subkey to query
                      iidstring, // LPTSTR lpName, // buffer for subkey name
                      &iidstring_buflen, // LPDWORD cbName   // size of subkey name buffer
		      NULL, NULL, NULL, NULL
                      );
      if (SUCCEEDED(hr))
        {
#ifdef _UNICODE
          char buf[MAX_PATH];
#if SICSTUS_TODO
#error "[PM] 4.0 Set and use UNICODE for this code instead! (wcsrtombs is locale dependent)"
#error "[PM] 4.0 error handling"
#endif  /* SICSTUS_TODO */
          /* Note: Windows wcsrtombs is thread safe (unlike POSIX) */
	  wcstombs(buf, iidstring, ((sizeof buf)/(sizeof buf[0])));
          buf[((sizeof buf)/(sizeof buf[0]))-1] = '\0';
          done = (*fun)(hKey, buf, userData);
#else  /* !_UNICODE */
          done = (*fun)(hKey, iidstring, userData);
#endif  /* !_UNICODE */
        }
    }
  
  // invariant done -> SUCCEEDED(hr)

 cleanup:
  if (hKey != HKEY_LOCAL_MACHINE)
    {
      (void) RegCloseKey(hKey);
    }

  return hr;
}
#endif  /* 0 */

// Instantiate an interface given a CLSID
static HRESULT SPCreateInstance(const CLSID *rclsid, const IID *pIID, IUnknown **ppItf)
{
  DWORD dwClsCtx = CLSCTX_ALL;
  DWORD dwClsCtxVB = CLSCTX_LOCAL_SERVER|CLSCTX_INPROC_SERVER;
  HRESULT hr = E_FAIL;
  int const old_way = 1;
  
  // For some reason CLSCTX_ALL will give 'No such interface' when asking Word about IDispatch
  // VB-to-VC claims dwClsCtxVB is what VB uses.
  dwClsCtx = dwClsCtxVB;

#ifndef CREATE_INSTANCE_VIA_IUNKNOWN
  /* [PM] 3.10.1 Some classes does not allow direct creation of
     IDispatch but does support getting IDispatch from IUnknown. In
     particular this happens for TLI.TLIApplication in VB98 (MS VS 6)
     Tlbinf32.dll */
#define CREATE_INSTANCE_VIA_IUNKNOWN 1
#endif /* CREATE_INSTANCE_VIA_IUNKNOWN */
  
  if (old_way)
    {
      IUnknown *pUnkn = NULL;
#if CREATE_INSTANCE_VIA_IUNKNOWN
      const IID *pIIDWanted = pIID;
      pIID = &IID_IUnknown;
#endif/* CREATE_INSTANCE_VIA_IUNKNOWN */

      hr = CoCreateInstance(
                            rclsid, // REFCLSID rclsid,     //Class identifier (CLSID) of the object
                            NULL, // LPUNKNOWN pUnkOuter, //Pointer to controlling IUnknown
                            dwClsCtx, // DWORD dwClsContext,  //Context for running executable code
                            pIID, // REFIID riid,         //Reference to the identifier of the interface
                            (void **)&pUnkn // LPVOID * ppv         //Address of output variable that receives 
                            //                            // the interface pointer requested in riid
                            );
      if (SUCCEEDED(hr))
        {
#if CREATE_INSTANCE_VIA_IUNKNOWN
          {
            /* We got a IUnknown, now try to get what we really wanted */
            IUnknown *pWanted = NULL;
            hr = IUnknown_QueryInterface(pUnkn, pIIDWanted, &pWanted);
            IUnknown_Release(pUnkn);
            pUnkn = pWanted; 
            if (!SUCCEEDED(hr))
              {
                /* No need to do anything, if !SUCCEEDED(hr) then
                   pWanted (and thus pUnkn) is garbage but pUnkn will
                   be ignored */
              }
          }
#endif/* CREATE_INSTANCE_VIA_IUNKNOWN */
          
          *ppItf = pUnkn;
        }
    }
#if !WINCE
  else
    {
      MULTI_QI results[1];

      results[0].pIID = pIID;
      results[0].pItf = NULL;
      results[0].hr = 0;
 
      hr = CoCreateInstanceEx(
                              rclsid, // REFCLSID rclsid,             //CLSID of the object to be created
                              NULL, // IUnknown *punkOuter,         //If part of an aggregate, the 
                              // controlling IUnknown
                              dwClsCtx, //  DWORD dwClsCtx,              //CLSCTX values
                              NULL, // COSERVERINFO *pServerInfo,   //Machine on which the object is to 
                              // be instantiated
                              1, // USP_INTEGER cmq,                   //Number of MULTI_QI structures in 
                              // pResults
                              results // MULTI_QI *pResults           //Array of MULTI_QI structures
                              );
      if (SUCCEEDED(hr))
        {
          *ppItf = results[0].pItf;
        }
    }
#endif  /* !WINCE */

  return hr;
}


// NOT USED YET
typedef struct sp_interface_ {
  IUnknown *pItf;
  // type-info, iid, ...
} sp_interface;

static void SPCDECL sp_interface_finalizer(void *obj_data, void *type_data)
{
  SP_integer tmp;
  IUnknown *pUnkn = (IUnknown *)obj_data;
#if COMCLIENT_DEBUG
  tmp = IUnknown_AddRef(pUnkn);
  tmp = IUnknown_Release(pUnkn);
  fprintf(stderr, "DBG COMCLIENT: IUnknown_Release(0x%" SPRIxINTEGER ") ref before == %" SPRIdINTEGER "\n", (SP_uinteger) pUnkn, (SP_integer) tmp);fflush(stderr);
#endif

  tmp = IUnknown_Release(pUnkn);

#if COMCLIENT_DEBUG
  fprintf(stderr, "DBG COMCLIENT: IUnknown_Release(0x%" SPRIxINTEGER ") ref after == %" SPRIdINTEGER "\n", (SP_uinteger) pUnkn, (SP_integer) tmp);fflush(stderr);
#endif
  
}

static int SPCDECL comclient_interface_putter(SP_term_ref tr, SP_external_object_link *obj)
{
  return SP_external_object_default_putter_helper(tr, obj, atom_comclient_object);
}


// Does AddRef!
static SP_external_object_link *register_iunknown(IUnknown *pUnkn)
{
  SP_external_object_link *pObj;
  pObj = SP_register_external_object(pUnkn, external_object_type__interface);
  if (pObj)
    {
      SP_integer tmp;
      tmp = IUnknown_AddRef(pUnkn);
#if COMCLIENT_DEBUG
      fprintf(stderr, "DBG COMCLIENT: IUnknown_AddRef(0x%" SPRIxINTEGER ") == %" SPRIdINTEGER "\n", (SP_uinteger) pUnkn, (SP_integer)tmp);fflush(stderr);
#endif
      
    }
  return pObj;
}

// Does AddRef!
static SP_external_object_link *register_idispatch(IDispatch *pDisp)
{
  SP_external_object_link *pObj = NULL;
  IUnknown *pUnkn = NULL;
  // for now store it as IUnknown

  if (FAILED(IDispatch_QueryInterface(pDisp, &IID_IUnknown, &pUnkn))) goto cleanup;
  pObj = register_iunknown(pUnkn); // Does pUnkn->AddRef
  
 cleanup:
  if (pUnkn)
    {
      SP_integer tmp;
      tmp = IUnknown_Release(pUnkn);

#if COMCLIENT_DEBUG
      fprintf(stderr, "DBG COMCLIENT: IUnknown_Release(0x%" SPRIxINTEGER ") == %" SPRIdINTEGER "\n", (SP_uinteger) pUnkn, (SP_integer) tmp);fflush(stderr);
#endif
    }
  return pObj;
}
     
#if 0
static IUnknown *lookup_iunknown(SP_integer obj)
{
  SP_external_object_link *pObj;

  pObj = (SP_external_object_link *)SP_find_external_object(obj);
  if (pObj
      && SP_get_external_object_type(pObj) == external_object_type__interface)
    {
      return (IUnknown *) SP_get_external_object_data(pObj);
    }
  else
    {
      return NULL;
    }
}
#endif

/* Does AddRef */
static IUnknown *comclient_get_interface(SP_term_ref tObj)
{
  IUnknown *pUnkn = NULL;
  SP_external_object_link *pObj;

  if (!SP_get_external_object(tObj, &pObj)) goto cleanup;
  if (SP_get_external_object_type(pObj) != external_object_type__interface)
    goto cleanup;
  
  pUnkn = (IUnknown *) SP_get_external_object_data(pObj);
  
  if (pUnkn) 
    {
      SP_integer tmp;
      tmp = IUnknown_AddRef(pUnkn); /* 21 Aug 2000 */
#if COMCLIENT_DEBUG
      fprintf(stderr, "DBG COMCLIENT: IUnknown_AddRef(0x%" SPRIxINTEGER ") == %" SPRIdINTEGER "\n", (SP_uinteger) pUnkn, (SP_integer) tmp);fflush(stderr);
#endif
    }      

 cleanup:
  return pUnkn;
}

static HRESULT SPGetCLSID(SP_term_ref tr, CLSID *pCLSID)
{
  HRESULT hr, hr1;
  BSTR bstr = NULL;
  CLSID clsid;

  if (!SPGetBSTR(tr, &bstr))
    {
      hr = DISP_E_TYPEMISMATCH; // Hmm
      goto cleanup;
    }
  
  hr = CLSIDFromString(bstr, &clsid);
  if (SUCCEEDED(hr)) goto cleanup;

  if (hr != CO_E_CLASSSTRING) goto cleanup;
  // improperly formatted, assume it is a ProgID
  hr1 = CLSIDFromProgID(bstr, &clsid);
  if (FAILED(hr1)) goto cleanup; // return the original failure code
  hr = hr1;

 cleanup:
  SysFreeString(bstr);
  if (SUCCEEDED(hr))
    {
      *pCLSID = clsid;
    }
  return hr;
}


static HRESULT SPGetIID(SP_term_ref tr, IID *pIID)
{
#if WINCE
  return S_FALSE;
#else  /* !WINCE */
  HRESULT hr, hr1;
  BSTR bstr = NULL;
  IID iid;

  if (!SPGetBSTR(tr, &bstr))
    {
      hr = DISP_E_TYPEMISMATCH; // Hmm
      goto cleanup;
    }
  

  hr = IIDFromString(bstr, &iid);
  if (SUCCEEDED(hr)) goto cleanup;
  // The docs does not spell out what error is returned on malformed.
  // improperly formatted (or other error), assume it is a interface name

  hr1 = INameToIIDU(bstr, &iid);
  if (FAILED(hr1)) goto cleanup; // return the original failure code
  hr = hr1;

 cleanup:
  SysFreeString(bstr);
  if (SUCCEEDED(hr))
    {
      *pIID = iid;
    }
  return hr;
#endif  /* !WINCE */
}

// comclient_create_instance(+CLSID, +IID, -ITF, -Result:integer).
// CLSID and IID are GUIDs in textual form (e.g., from SP_CLSIDStringFromProgID
void SPCDECL comclient_create_instance(SP_term_ref tCLSID, SP_term_ref tIID, SP_term_ref tItf, SP_integer *result)
{
  CLSID clsid;
  IID iid;
  IUnknown *pItf = NULL;
  SP_external_object_link *pObj = NULL;
  HRESULT hr;

  EntryBreakPoint("comclient_create_instance"); 

  hr = SPGetCLSID(tCLSID, &clsid);
  if (!SUCCEEDED(hr)) goto cleanup;

  hr = SPGetIID(tIID, &iid);
  if (!SUCCEEDED(hr)) goto cleanup;

  hr = SPCreateInstance(&clsid, &iid, &pItf);
  if (!SUCCEEDED(hr)) goto cleanup;


  pObj = register_iunknown(pItf); // does AddRef on pItf

  if (!pObj)
    {
      hr = DISP_E_TYPEMISMATCH; // Hmm
      goto cleanup;
    }
  if (!SP_put_external_object(tItf, pObj))
    {
      hr = DISP_E_TYPEMISMATCH; // Hmm
      goto cleanup;
    }

  pObj = NULL; // prevent kill in cleanup

 cleanup:
  if (pItf)
    {
      SP_integer tmp;
      tmp = IUnknown_Release(pItf);
#if COMCLIENT_DEBUG
      fprintf(stderr, "DBG COMCLIENT: IUnknown_Release(0x%" SPRIxINTEGER ") == %" SPRIdINTEGER "\n", (SP_uinteger) pItf, (SP_integer) tmp);fflush(stderr);
#endif      
    }
  if (pObj)
    {
      (void) SP_unlink_external_object(pObj, 1/*finalize*/);
    }

  *result = hr;
}


void SPCDECL comclient_get_active_object(SP_term_ref tCLSID, SP_term_ref tIID, SP_term_ref tItf, SP_integer *result)
{
#if !WINCE
  CLSID clsid;
  IID iid;
  IUnknown *pItf = NULL, *pUnkn = NULL;
  SP_external_object_link *pObj = NULL;
  HRESULT hr;

  EntryBreakPoint("comclient_get_active_object"); 

  hr = SPGetCLSID(tCLSID, &clsid);
  if (!SUCCEEDED(hr)) goto cleanup;

  hr = SPGetIID(tIID, &iid);
  if (!SUCCEEDED(hr)) goto cleanup;

  // hr = SPCreateInstance(&clsid, &iid, &pItf);
  hr = GetActiveObject(&clsid, NULL, &pUnkn);
  if (!SUCCEEDED(hr)) goto cleanup;

  hr = IUnknown_QueryInterface(pUnkn, &iid, &pItf);
  if (!SUCCEEDED(hr)) goto cleanup;

  pObj = register_iunknown(pItf); // does AddRef on pItf

  if (!pObj)
    {
      hr = DISP_E_TYPEMISMATCH; // Hmm
      goto cleanup;
    }
  if (!SP_put_external_object(tItf, pObj))
    {
      hr = DISP_E_TYPEMISMATCH; // Hmm
      goto cleanup;
    }

  pObj = NULL; // prevent kill in cleanup

 cleanup:
  if (pItf)
    {
      SP_integer tmp;
      tmp = IUnknown_Release(pItf);
#if COMCLIENT_DEBUG
      fprintf(stderr, "DBG COMCLIENT: IUnknown_Release(0x%" SPRIxINTEGER ") == %" SPRIdINTEGER "\n", (SP_uinteger) pItf, (SP_integer) tmp);fflush(stderr);
#endif
    }
  if (pUnkn)
    {
      SP_integer tmp;
      tmp = IUnknown_Release(pUnkn);
#if COMCLIENT_DEBUG
      fprintf(stderr, "DBG COMCLIENT: IUnknown_Release(0x%" SPRIxINTEGER ") == %" SPRIdINTEGER "\n", (SP_uinteger) pUnkn, (SP_integer) tmp);fflush(stderr);
#endif
    }
  if (pObj)
    {
      (void) SP_unlink_external_object(pObj, 1/*finalize*/);
    }

  *result = hr;
#else  /* WINCE */
    // [PM] 3.11.2 FIXME: CN wrote: Some of the COM functions used
    // here DO exist in some CE incarnations, but not the Pocket PC
    // one.  We should have some kind of addition to configure to
    // accomodate for that.
  *result = S_FALSE;
#endif  /* WINCE */
}


// Ugly. Later should cache ITypeInfo and also look in type libs
static ITypeInfo *get_object_type_info(SP_external_object_link *pObj,
         int dispatch)
{
  sp_interface *pSPI = NULL;
  IUnknown *pUnkn = NULL;
  IDispatch *pDsp = NULL;
  ITypeInfo *pTInfo = NULL;
  TYPEATTR *pTypeAttr = NULL;
  int count;
  HRESULT hr = S_OK;

  pSPI = (sp_interface *) SP_get_external_object_data(pObj);

  pUnkn = pSPI->pItf;

  hr = IUnknown_QueryInterface(pUnkn, &IID_IDispatch, &pDsp);
  if (!SUCCEEDED(hr)) goto cleanup;

  hr = IDispatch_GetTypeInfoCount(pDsp, &count);
  if (!SUCCEEDED(hr)) goto cleanup;

  if ( !(count > 0) )
    {
      goto cleanup;
    }

  hr = IDispatch_GetTypeInfo(pDsp, 0, defaultLCID, &pTInfo);
  if (!SUCCEEDED(hr)) goto cleanup;


  hr = ITypeInfo_GetTypeAttr(pTInfo, &pTypeAttr);
  if (!SUCCEEDED(hr)) goto cleanup;


#if 0 // Sample code seems to indicate that TKIND_DISPATCH is indeed the default
  /*

    What this SHOULD do when 'dispatch' is true is to check if
    TYPEFLAG_FDUAL and in TKIND_INTERFACE and in that case do the
    GetRefTypeOfImplType -1 trick. Otherwise, unless TKIND_DISPATCH it 
    should err and return NULL.

   */
  if (dispatch && pTypeAttr->typekind != TKIND_DISPATCH)
    {
      /* 
         Contradictory documentation (MSDN January 2000):

         Documentation from 
         "Type Description Interfaces\ITypeInfo Interface\Type
         Descriptions":

         Dual interfaces (dual) have two different type descriptions
         for the same interface. The TKIND_INTERFACE type description
         describes the interface as a standard OLE Component Object
         Model (COM) interface. The TKIND_DISPATCH type description
         describes the interface as a standard dispatch interface. The
         lcid and retval parameters, and the HRESULT return types are
         removed, and the return type of the member is specified to be
         the same type as the retval parameter.

         By default, the TYPEKIND enumeration for a dual interface is
         TKIND_INTERFACE. Tools that bind to interfaces should check
         the type flags for TYPEFLAG_FDUAL. If this flag is set, the
         TKIND_DISPATCH type description is available through a call
         to ITypeInfo::GetRefTypeOfImplType with an index of -1,
         followed by a call to ITypeInfo::GetRefTypeInfo.

         Documentation of TYPEKIND:

         TKIND_DISPATCH A set of methods and properties that are
         accessible through IDispatch::Invoke. By default, dual
         interfaces return TKIND_DISPATCH.

         Documentation of ITypeInfo::GetRefTypeOfImplType:
         
         If the TKIND_DISPATCH type description is for a dual
         interface, the TKIND_INTERFACE type description can be
         obtained by calling GetRefTypeOfImplType with an index of -1,
         and by passing the returned pRefType handle to GetRefTypeInfo
         to retrieve the type information.


      */

      if (pTypeAttr->typekind == TKIND_INTERFACE
          && pTypeAttr->wTypeFlags & TKIND_FDUAL )
        {
          HREFTYPE refType;
          ITypeInfo *pTInfoTmp = NULL;
          SP_integer tmp;
        
        

          hr = ITypeInfo_GetRefTypeOfImplType(pTInfo, -1, &refType);
          if (!SUCCEEDED(hr)) goto cleanup;
          hr = ITypeInfo_GetRefTypeInfo(pTInfo, refType, &pTInfoTmp);
          if (!SUCCEEDED(hr)) goto cleanup;
          tmp = ITypeInfo_Release(pTInfo);
#if COMCLIENT_DEBUG
          fprintf(stderr, "DBG COMCLIENT: ITypeInfo_Release(0x%" SPRIxINTEGER ") == %" SPRIdINTEGER "\n", (SP_uinteger) pTInfo, (SP_integer)tmp);fflush(stderr);
#endif
          
          pTInfo = pTInfoTmp;
        }
      else
        {
          hr = E_NOINTERFACE; // not ideal
          goto cleanup;
        }
    }
#endif


 cleanup:

  if (pDsp)
    {
      SP_integer tmp;
      tmp = IDispatch_Release(pDsp);

#if COMCLIENT_DEBUG
      fprintf(stderr, "DBG COMCLIENT: IDispatch_Release(0x%" SPRIxINTEGER ") == %" SPRIdINTEGER "\n", (SP_uinteger) pDsp, (SP_integer)tmp);fflush(stderr);
#endif
      
    }
  if (pTInfo)
    {
      if (pTypeAttr)
        {
          ITypeInfo_ReleaseTypeAttr(pTInfo, pTypeAttr);
        }
    }

  return pTInfo;
}

static ITypeInfo *get_type_info(SP_external_object_link *pObj)
{
  return get_object_type_info(pObj, 0);
}

static ITypeInfo *get_dispatch_type_info_info(SP_external_object_link *pObj)
{
  return get_object_type_info(pObj, 1);
}

static HRESULT sp_get_dispparams(FUNCDESC *pFuncDesc,
                                 SP_term_ref tArgList,
                                 SP_term_ref tNamedArgs,
                                 // WORD wFlags,
                                 SP_term_ref tPutValue /* only if wFlags = PUT_... */
                                 )
{
  MEMBERID memid;
  INVOKEKIND invkind;
  short cParams;
  short cParamsOpt;
  ELEMDESC *pElemdescParam;
  ELEMDESC elemdescFunc;
  WORD wFuncFlags;
  HRESULT hr = S_OK;

  // paranoia
  if ( !( pFuncDesc->funckind == FUNC_PUREVIRTUAL
          || pFuncDesc->funckind == FUNC_VIRTUAL
          || pFuncDesc->funckind == FUNC_DISPATCH ) )
    {
      hr = E_INVALIDARG;
      goto cleanup;
    }

  invkind = pFuncDesc->invkind;
  memid = pFuncDesc->memid;   // Function member ID
  cParams = pFuncDesc->cParams; // Count of total number of parameters
  cParamsOpt = pFuncDesc->cParamsOpt; // Count of optional parameters
  pElemdescParam = pFuncDesc->lprgelemdescParam; // Argument types
  elemdescFunc = pFuncDesc->elemdescFunc;    // Contains the return type of the function.
  wFuncFlags = pFuncDesc->wFuncFlags;

 cleanup:
  return hr;
}

static HRESULT SPGetVariant(SP_term_ref t, VARIANT *pVar, VARIANT *pVarByRef)
{
  HRESULT hr = S_OK;

  (void) VariantInit(pVar);
  if (pVarByRef)
    {
      (void) VariantInit(pVarByRef);
      V_VT(pVarByRef) = (VT_VARIANT | VT_BYREF);
      V_VARIANTREF(pVarByRef) = pVar;
    }

  switch (SP_term_type(t))
    {
    case SP_TYPE_VARIABLE:
      // If by ref then we hope it's an out arg and the method will
      // not see the VT_ERROR
      if (!pVarByRef)
        {
          hr = E_INVALIDARG;        /* for now */
        }
      V_VT(pVar) = VT_ERROR;
      V_ERROR(pVar) = hr;
      break;
    case SP_TYPE_INTEGER:
      {
	SP_integer tmp = 0;
	if (!SP_get_integer(t, &tmp))
	  {
	    hr = DISP_E_OVERFLOW;
	    V_VT(pVar) = VT_ERROR;
	    V_ERROR(pVar) = hr;
	  }
	V_VT(pVar) = VT_I4;
	V_I4(pVar) = (LONG) tmp;
      }
      break;
    case SP_TYPE_FLOAT:
      V_VT(pVar) = VT_R8;
      if (!SP_get_float(t, &V_R8(pVar)))
        { // cannot happen
          hr = DISP_E_OVERFLOW;      // Hmm
          V_VT(pVar) = VT_ERROR;
          V_ERROR(pVar) = hr;
        }
      break;
    case SP_TYPE_COMPOUND:
      if (!SP_is_list(t))
        {
          IUnknown *pUnkn = NULL;
          pUnkn = comclient_get_interface(t); /* safe for any valid term-ref */
          if (pUnkn)
            {
              V_VT(pVar) = VT_UNKNOWN;
              V_UNKNOWN(pVar) = pUnkn;
            }
          else
            {
              hr = E_INVALIDARG;        /* for now */
              V_VT(pVar) = VT_ERROR;
              V_ERROR(pVar) = hr;
            }
          break;
        }
      else
        {
          // FALL THROUGH to SPGetBSTR (assuming list of char codes)
        }

    case SP_TYPE_ATOM:
      V_VT(pVar) = VT_BSTR;
      if (!SPGetBSTR(t, &V_BSTR(pVar))) // handles list of char codes too
        {
          hr = DISP_E_OVERFLOW;      // Hmm
          V_VT(pVar) = VT_ERROR;
          V_ERROR(pVar) = hr;
        }
      break;
    }

  return hr;
}

// Does not clear pVar
static HRESULT SPPutVariant(SP_term_ref t, VARIANT *pVar)
{
  HRESULT hr = DISP_E_TYPEMISMATCH;
  SP_external_object_link *pObj = NULL;
  VARIANT var;

  VariantInit(&var);

  switch (V_VT(pVar) & (~(VARTYPE)VT_BYREF)) // mask away VT_BYREF [PM] 3.10.2 but *not* VT_ARRAY
    {
    case VT_UI8:
    case VT_I8:
    case VT_I4:
    case VT_UI1:
    case VT_I2:
    case VT_I1:
    case VT_UI2:
    case VT_UI4:
    case VT_INT:
    case VT_UINT:
      hr = VariantChangeType(&var, pVar, 0, VT_I4);
      if (FAILED(hr)) goto cleanup;
      if (!SP_put_integer(t, V_I4(&var)))
        {
          hr = DISP_E_OVERFLOW; // Hmm
          goto cleanup;
        }
      break;

    case VT_R4:
    case VT_R8:
      hr = VariantChangeType(&var, pVar, 0, VT_R8);
      if (FAILED(hr)) goto cleanup;
      if (!SP_put_float(t, V_R8(&var)))
        {
          hr = DISP_E_OVERFLOW; // Hmm
          goto cleanup;
        }
      break;
    case VT_BOOL:
      {
        VARIANT_BOOL b = (V_ISBYREF(pVar) ? *V_BOOLREF(pVar) : V_BOOL(pVar));
        if (!SP_put_integer(t, (SP_integer)(b != VARIANT_FALSE)))
          {
            hr = DISP_E_OVERFLOW; // Hmm
            goto cleanup;
          }
        hr = S_OK;
        break;
      }
    case VT_ERROR:
      {
        // If  SUCCEEDED then what?
        if (FAILED(V_ERROR(pVar)))
          {
            hr = V_ERROR(pVar);
          }
        goto cleanup;
      }
    case VT_CY:
    case VT_DATE:
      // For now try to make them strings

      // FALLTHROUGH
    case VT_BSTR:
      hr = VariantChangeType(&var, pVar, 0, VT_BSTR);
      if (FAILED(hr)) goto cleanup;
      if (!SPPutBSTR(t, V_BSTR(&var), 0)) // 0 means not as atom (reconsider this?)
        {
          hr = DISP_E_OVERFLOW; // Hmm
          goto cleanup;
        }
      break;
    case VT_UNKNOWN:
      {
        IUnknown *pUnkn = (V_ISBYREF(pVar) ?
                           ( (V_UNKNOWNREF(pVar) == NULL) ? NULL : *V_UNKNOWNREF(pVar) ) /* paranoia */
                           : V_UNKNOWN(pVar));

        if (pUnkn == NULL)      /* [PM] 3.10.1 see VT_DISPATCH for why this precaution is taken */
          {
            hr = DISP_E_TYPEMISMATCH; /* FIXME: */
            goto cleanup;
          }
        pObj = register_iunknown(pUnkn); // does AddRef
        if (!pObj)
          {
            hr = DISP_E_OVERFLOW; // Hmm
            goto cleanup;
          }
        if (!SP_put_external_object(t, pObj))
          {
            hr = DISP_E_OVERFLOW; // Hmm
            goto cleanup;
          }
        pObj = NULL;            /* protect from cleanup */
        hr = S_OK;
      }

    case VT_DISPATCH:
      {
        IDispatch *pDisp = ((V_ISBYREF(pVar) ? 
                             ( (V_DISPATCHREF(pVar) == NULL) ? NULL : *V_DISPATCHREF(pVar) ) /* paranoia */
                             : V_DISPATCH(pVar)));

        /* [PM] 3.10.1b1 V_DISPATCH(pVar)==NULL happens for
           Word.Application.selection when there is no open document.
           (encountered during PRM 4401)
        */
        if (pDisp == NULL)
          {
            hr = DISP_E_TYPEMISMATCH; /* FIXME:  */
            goto cleanup;
          }
        pObj = register_idispatch(pDisp); // does AddRef
        if (!pObj)
          {
            hr = DISP_E_OVERFLOW; // Hmm
            goto cleanup;
          }
        if (!SP_put_external_object(t, pObj))
          {
            hr = DISP_E_OVERFLOW; // Hmm
            goto cleanup;
          }
        pObj = NULL;            /* protect from cleanup */
        hr = S_OK;
      }
      break;

    default:
      // case VT_ARRAY:              // barf for now
      // case 0: // (Generic ByRef) barf for now
      hr = DISP_E_TYPEMISMATCH;
      goto cleanup;
      break;
    }

 cleanup:
  if (pObj)
    {
      (void) SP_unlink_external_object(pObj, 1/*finalize*/);
    }

  VariantClear(&var);
  return hr;
}

// Bits outside range of DISPATCH_METHOD et al
#define SPCOM_FLAG_RETVAL 64    /* xref comclient.pl */

#if 0                           /* [PM] 3.10.1b1 now use SPInvokeEx_helper */
   // Returns S_OK to mean SP_SUCCESS, S_FALSE to mean SP_FAILURE and E_XXX to indicate SP_ERROR
   static HRESULT SPInvoke(SP_term_ref tObj, SP_term_ref tGoal, SP_term_ref tFlags, SP_term_ref tAnswer)
   {
     HRESULT hr = E_INVALIDARG;
     int i;

     IUnknown *pUnkn = NULL;
     IDispatch *pDsp = NULL;

     SP_integer flags;
     WORD wFlags;

     SP_atom atom;
     int arity;
     char *atom_name;
     BSTR methodName = NULL;
     char *method = NULL;
     DISPID dispid;
     VARIANT varResult, *pVarResult = NULL;
     DISPID dispidNamed = DISPID_PROPERTYPUT;
     DISPPARAMS dispParams;
     EXCEPINFO excepInfo, *pExcepInfo = &excepInfo;
     unsigned int argErr, *puArgErr = &argErr;

     // make it safe to cleanup
     excepInfo.bstrSource = NULL;
     excepInfo.bstrDescription = NULL;
     excepInfo.bstrHelpFile = NULL;

     VariantInit(&varResult);

     dispParams.rgvarg = NULL;
     dispParams.rgdispidNamedArgs = NULL;
     dispParams.cArgs = 0;
     dispParams.cNamedArgs = 0;

     pUnkn = comclient_get_interface(tObj); // Addref
     if (!pUnkn) goto cleanup;

     if (!SP_get_integer(tFlags, &flags)) goto cleanup;

     wFlags = 0;
     if (flags & DISPATCH_METHOD) wFlags |= DISPATCH_METHOD;
     if (flags & DISPATCH_PROPERTYGET) wFlags |= DISPATCH_PROPERTYGET;
     if (flags & DISPATCH_PROPERTYPUT) wFlags |= DISPATCH_PROPERTYPUT;
     if (flags & DISPATCH_PROPERTYPUTREF) wFlags |= DISPATCH_PROPERTYPUTREF;

     if (flags & SPCOM_FLAG_RETVAL)
       {
         pVarResult = &varResult;
       }
     if ( wFlags & DISPATCH_PROPERTYPUT
         || wFlags & DISPATCH_PROPERTYPUTREF )
       {
         dispParams.cNamedArgs = 1;
         dispParams.rgdispidNamedArgs = &dispidNamed;
       }

     // Later keep track of this ourselves
     hr = IUnknown_QueryInterface(pUnkn, &IID_IDispatch, &pDsp);
     if (FAILED(hr)) goto cleanup;

     if (!SP_get_functor(tGoal, &atom, &arity)) goto cleanup;
     // if (!SP_put_functor(tAnswer, atom, (arity + (pVarResult ? 1 : 0)))) goto cleanup;

     atom_name = SP_string_from_atom(atom);

     if (!SysAllocStringSP(atom_name, &methodName)) goto cleanup;
     // atom no SP_integerer used, no need to register it

     hr = IDispatch_GetIDsOfNames(pDsp,&IID_NULL, &methodName, 1, defaultLCID, &dispid);
     if (FAILED(hr)) goto cleanup;

     // Why not use atom? Because I was testing something.
     if (!SysAllocSPTmpString(methodName, &method)) goto cleanup;
     if (!SP_put_functor(tAnswer, SP_atom_from_string(method), (arity + (pVarResult ? 1 : 0)))) goto cleanup;

     dispParams.cArgs = arity;

     if (arity > 0)
       {
         dispParams.rgvarg = CoTaskMemAlloc((2*arity)*sizeof dispParams.rgvarg[0]);
         if (!dispParams.rgvarg)
           {
             hr = E_OUTOFMEMORY;
             goto cleanup;
           }
       }

     // Make cleanup safe
     for (i = 0; i < (2*arity); i++)
       {
         VariantInit(&dispParams.rgvarg[i]);
       }

     // Uses tObj as a temporary. This avoids allocating a new term ref
     // Traverse Goal by decreasing index to make up for the fact that
     // the args to IDispatch::Invoke are stored in reverse order.
     for (i = 0; i < arity; i++)
       {
         HRESULT hrArg;
         if (!SP_get_arg(arity-i, tGoal, tObj)) // one based!
           {
             goto cleanup;
           }
         // The value is put in dispParam.rgvarg[i+arity] and a VT_REF to 
         // it is passed in dispParam.rgvarg[i] as the argument to Invoke 
         hrArg = SPGetVariant(tObj, &dispParams.rgvarg[i+arity], &dispParams.rgvarg[i]);
         if (FAILED(hrArg))
           {
             hr = hrArg;
             goto cleanup;
           }
       }

     // wFlags = (DISPATCH_METHOD | DISPATCH_PROPERTYGET); // for now

     pExcepInfo = NULL;            // for now
     puArgErr = NULL;              // for now
     hr = IDispatch_Invoke(pDsp,
                           dispid,
                           &IID_NULL,
                           defaultLCID,
                           wFlags,
                           &dispParams,
                           pVarResult,
                           pExcepInfo,
                           puArgErr);
     if (FAILED(hr))
       {
         // do elaborate exception handling later
         goto cleanup;
       }

     for (i = 0; i < arity; i++)
       {
         // By now tObj and tGoal are unused and can be reused as temporaries
         SP_term_ref tArg = tObj, tVariant = tGoal;

         if (!SP_get_arg(arity-i, tAnswer, tArg)) // one based!
           {
             goto cleanup;
           }
         hr = SPPutVariant(tVariant, &dispParams.rgvarg[i+arity]);
         if (FAILED(hr)) goto cleanup;

         if (!SP_unify(tArg, tVariant))
           { // Should not happen as tArg is a variable
             hr = S_FALSE;    // Signal failure
             goto cleanup;
           }
       }

     if (pVarResult)
       {
         SP_term_ref tReturnArg = tObj, tReturnVariant = tGoal;
         if (!SP_get_arg(arity+1, tAnswer, tReturnArg)) // one based!
           {
             goto cleanup;
           }
         hr = SPPutVariant(tReturnVariant, pVarResult);
         if (FAILED(hr)) goto cleanup;

         if (!SP_unify(tReturnArg, tReturnVariant))
           { // Should not happen as tReturnArg is a variable
             hr = S_FALSE;    // Signal failure
             goto cleanup;
           }
       }
     // We get here if everything went well
     hr = S_OK;

    cleanup:

     /*
       if SP_ERR then should use hr to create an exception (if FAILED(hr), otherwise generic error).
     */

     if (dispParams.rgvarg)
       {
         // These are all inited if allocated
         for (i = 0; i < (2*arity); i++)
           {
             (void) VariantClear(&dispParams.rgvarg[i]);
           }
         CoTaskMemFree(dispParams.rgvarg);
       }
     if (pVarResult)
       {
         (void) VariantClear(pVarResult);
       }
     if (pDsp)
       {
         SP_integer tmp;
         tmp = IDispatch_Release(pDsp);
#if COMCLIENT_DEBUG
         fprintf(stderr, "DBG COMCLIENT: IDispatch_Release(0x%" SPRIxINTEGER ") == %" SPRIdINTEGER "\n", (SP_uinteger) pDsp, (SP_integer)tmp);fflush(stderr);
#endif
       }
     if (pUnkn)
       {
         SP_integer tmp;
         tmp = IUnknown_Release(pUnkn);
#if COMCLIENT_DEBUG
         fprintf(stderr, "DBG COMCLIENT: IUnknown_Release(0x%" SPRIxINTEGER ") == %" SPRIdINTEGER "\n", (SP_uinteger) pUnkn, (SP_integer)tmp);fflush(stderr);
#endif      
       }
     if (methodName) SysFreeString(methodName);
     if (method) FreeTmpString(method);

     return hr;
   }
#endif /* 0 */

// Returns S_OK to mean SP_SUCCESS, S_FALSE to mean SP_FAILURE and E_XXX to indicate SP_ERROR
static HRESULT SPInvokeEx_helper(SP_term_ref tObj, SP_term_ref tGoal, SP_term_ref tFlags, SP_term_ref tAnswer, SP_term_ref tResult, int have_tResult)
{
  HRESULT hr = E_INVALIDARG;
  int i;

  IUnknown *pUnkn = NULL;
  IDispatch *pDsp = NULL;
  
  SP_integer flags;
  WORD wFlags;

  SP_atom atom;
  int arity;
  char const *atom_name;
  BSTR methodName = NULL;
  char *method = NULL;
  DISPID dispid;
  VARIANT varResult, *pVarResult = NULL;
  DISPID dispidNamed = DISPID_PROPERTYPUT;
  DISPPARAMS dispParams;
  EXCEPINFO excepInfo, *pExcepInfo = &excepInfo;
  unsigned int argErr, *puArgErr = &argErr;

  // make it safe to cleanup
  excepInfo.bstrSource = NULL;
  excepInfo.bstrDescription = NULL;
  excepInfo.bstrHelpFile = NULL;

  VariantInit(&varResult);

  dispParams.rgvarg = NULL;
  dispParams.rgdispidNamedArgs = NULL;
  dispParams.cArgs = 0;
  dispParams.cNamedArgs = 0;

  pUnkn = comclient_get_interface(tObj); // Addref
  if (!pUnkn) goto cleanup;

  if (!SP_get_integer(tFlags, &flags)) goto cleanup;
  
  wFlags = 0;
  if (flags & DISPATCH_METHOD) wFlags |= DISPATCH_METHOD;
  if (flags & DISPATCH_PROPERTYGET) wFlags |= DISPATCH_PROPERTYGET;
  if (flags & DISPATCH_PROPERTYPUT) wFlags |= DISPATCH_PROPERTYPUT;
  if (flags & DISPATCH_PROPERTYPUTREF) wFlags |= DISPATCH_PROPERTYPUTREF;

  if (flags & SPCOM_FLAG_RETVAL)
    {
      pVarResult = &varResult;
    }
  if ( wFlags & DISPATCH_PROPERTYPUT
      || wFlags & DISPATCH_PROPERTYPUTREF )
    {
      dispParams.cNamedArgs = 1;
      dispParams.rgdispidNamedArgs = &dispidNamed;
    }

  // Later keep track of this ourselves
  hr = IUnknown_QueryInterface(pUnkn, &IID_IDispatch, &pDsp);
  if (FAILED(hr)) goto cleanup;

  if (!SP_get_functor(tGoal, &atom, &arity)) goto cleanup;
  if (have_tResult && !SP_put_functor(tAnswer, atom, (arity + (pVarResult ? 1 : 0)))) goto cleanup;

  atom_name = SP_string_from_atom(atom);

  if (!SysAllocStringSP(atom_name, &methodName)) goto cleanup;
  // atom no SP_integerer used, no need to register it
  
  hr = IDispatch_GetIDsOfNames(pDsp,&IID_NULL, &methodName, 1, defaultLCID, &dispid);
  if (FAILED(hr)) goto cleanup;

  if (!have_tResult)
    {
      // Why not use atom? Because I was testing something.
      if (!SysAllocSPTmpString(methodName, &method)) goto cleanup;
      if (!SP_put_functor(tAnswer, SP_atom_from_string(method), (arity + (pVarResult ? 1 : 0)))) goto cleanup;
    }

  argErr = arity+1;             // impossible value.
  dispParams.cArgs = arity;

  if (arity > 0)
    {
      dispParams.rgvarg = CoTaskMemAlloc((2*arity)*sizeof dispParams.rgvarg[0]);
      if (!dispParams.rgvarg)
        {
          hr = E_OUTOFMEMORY;
          goto cleanup;
        }
    }

  // Make cleanup safe
  for (i = 0; i < (2*arity); i++)
    {
      VariantInit(&dispParams.rgvarg[i]);
    }

  // Uses tObj as a temporary. This avoids allocating a new term ref
  // Traverse Goal by decreasing index to make up for the fact that
  // the args to IDispatch::Invoke are stored in reverse order.
  for (i = 0; i < arity; i++)
    {
      HRESULT hrArg;
      if (!SP_get_arg(arity-i, tGoal, tObj)) // one based!
        {
          goto cleanup;
        }
      // The value is put in dispParam.rgvarg[i+arity] and a VT_REF to 
      // it is passed in dispParam.rgvarg[i] as the argument to Invoke 
      hrArg = SPGetVariant(tObj, &dispParams.rgvarg[i+arity], &dispParams.rgvarg[i]);
      if (FAILED(hrArg))
        {
          hr = hrArg;
          goto cleanup;
        }
    }

  // wFlags = (DISPATCH_METHOD | DISPATCH_PROPERTYGET); // for now

  puArgErr = NULL;              // for now
  hr = IDispatch_Invoke(pDsp,
                        dispid,
                        &IID_NULL,
                        defaultLCID,
                        wFlags,
                        &dispParams,
                        pVarResult,
                        pExcepInfo,
                        puArgErr);
  if (FAILED(hr))
    {
      if (hr == DISP_E_EXCEPTION)
        {
          // By now tObj and tGoal are unused and can be reused as temporaries
          SP_term_ref tArg = tObj, tTmp = tGoal;
          int argNo;

          if (pExcepInfo->pfnDeferredFillIn)
            {
              (*pExcepInfo->pfnDeferredFillIn)(pExcepInfo);
            }

          argNo = 1;
          if (!(
                SP_put_functor(tResult, SP_atom_from_string("EXCEPINFO"), 6)
                && SP_get_arg(argNo++, tResult, tArg)
                && SP_put_integer(tTmp, (SP_integer) pExcepInfo->wCode)
                && SP_unify(tArg, tTmp)
                && SP_get_arg(argNo++, tResult, tArg)
                && SPPutBSTR(tTmp, pExcepInfo->bstrSource, 1)
                && SP_unify(tArg, tTmp)
                && SP_get_arg(argNo++, tResult, tArg)
                && SPPutBSTR(tTmp, pExcepInfo->bstrDescription, 1)
                && SP_unify(tArg, tTmp)
                && SP_get_arg(argNo++, tResult, tArg)
                && SPPutBSTR(tTmp, pExcepInfo->bstrHelpFile, 1)
                && SP_unify(tArg, tTmp)
                && SP_get_arg(argNo++, tResult, tArg)
                && SP_put_integer(tTmp, (SP_integer) pExcepInfo->dwHelpContext)
                && SP_unify(tArg, tTmp)
                && SP_get_arg(argNo++, tResult, tArg)
                && SP_put_integer(tTmp, (SP_integer) pExcepInfo->scode)
                && SP_unify(tArg, tTmp)
                ))
            {
              goto cleanup;
            }
          goto cleanup;
        }
      else // failed but no exception
        {
          if ((hr == DISP_E_PARAMNOTFOUND || hr == DISP_E_TYPEMISMATCH)
              && argErr <= (unsigned int) arity )
            {
              // tObj and tGoal unused, reuse as temporaries
              SP_term_ref tArg = tObj, tTmp = tGoal;
            
              if (!(
                    // 'ARGERR'(methodFoo, 2)
                    SP_put_functor(tResult, SP_atom_from_string("ARGERR"), 1)

                    && SP_get_arg(1, tResult, tArg)
                    && SPPutBSTR(tTmp, methodName, 1)
                    && SP_unify(tArg, tTmp)

                    && SP_get_arg(2, tResult, tArg)
                    && SP_put_integer(tTmp, (SP_integer) argErr)
                    && SP_unify(tArg, tTmp)
                    ))
                {
#if COMCLIENT_DEBUG
                  fprintf(stderr, "ERROR COMCLIENT: %s:%d\n", __FILE__, (int)__LINE__);fflush(stderr);
#endif/* COMCLIENT */
                  ;
                }
              goto cleanup;
            }
          else                    // some other error */
            {
              goto cleanup;
            }
        }

      // NOTREACHED
    }
  /* hr is success here */
  for (i = 0; i < arity; i++)
    {
      // By now tObj and tGoal are unused and can be reused as temporaries
      SP_term_ref tArg = tObj, tVariant = tGoal;

      if (!SP_get_arg(arity-i, tAnswer, tArg)) // one based!
        {
          hr = E_INVALIDARG;    /* FIXME: */
          goto cleanup;
        }
      hr = SPPutVariant(tVariant, &dispParams.rgvarg[i+arity]);
      if (FAILED(hr)) goto cleanup;

      if (!SP_unify(tArg, tVariant))
        { // Should not happen as tArg is a variable
          hr = S_FALSE;    // Signal failure
          goto cleanup;
        }
    }
  /* hr is success here */
  if (pVarResult)
    {
      SP_term_ref tReturnArg = tObj, tReturnVariant = tGoal;
      if (!SP_get_arg(arity+1, tAnswer, tReturnArg)) // one based!
        {
          hr = E_INVALIDARG;    /* FIXME: */
          goto cleanup;
        }
      hr = SPPutVariant(tReturnVariant, pVarResult);
      if (FAILED(hr)) goto cleanup;

      if (!SP_unify(tReturnArg, tReturnVariant))
        { // Should not happen as tReturnArg is a variable
          hr = S_FALSE;    // Signal failure
          goto cleanup;
        }
    }
  // We get here if everything went well
  hr = S_OK;

 cleanup:

  /*
    [PM] 3.10.1 FIXME: what does this comment mean?
    if SP_ERR then should use hr to create an exception (if FAILED(hr), otherwise generic error).
  */
  
  if (dispParams.rgvarg)
    {
      // These are all inited if allocated
      for (i = 0; i < (2*arity); i++)
        {
          (void) VariantClear(&dispParams.rgvarg[i]);
        }
      CoTaskMemFree(dispParams.rgvarg);
    }
  if (pVarResult)
    {
      (void) VariantClear(pVarResult);
    }
  if (pDsp)
    {
      SP_integer tmp;
      tmp = IDispatch_Release(pDsp);
#if COMCLIENT_DEBUG
      fprintf(stderr, "DBG COMCLIENT: IDispatch_Release(0x%" SPRIxINTEGER ") == %" SPRIdINTEGER "\n", (SP_uinteger) pDsp, (SP_integer) tmp);fflush(stderr);
#endif
    }
  if (pUnkn)
    {
      SP_integer tmp;
      tmp = IUnknown_Release(pUnkn);
#if COMCLIENT_DEBUG
      fprintf(stderr, "DBG COMCLIENT: IUnknown_Release(0x%" SPRIxINTEGER ") == %" SPRIdINTEGER "\n", (SP_uinteger) pUnkn, (SP_integer) tmp);fflush(stderr);
#endif      
    }
  if (methodName) SysFreeString(methodName);
  if (method) FreeTmpString(method);
  if (excepInfo.bstrSource) SysFreeString(excepInfo.bstrSource);
  if (excepInfo.bstrDescription)SysFreeString(excepInfo.bstrDescription);
  if (excepInfo.bstrHelpFile) SysFreeString(excepInfo.bstrHelpFile);

  return hr;
}

// Returns S_OK to mean SP_SUCCESS, S_FALSE to mean SP_FAILURE and E_XXX to indicate SP_ERROR
static HRESULT SPInvoke(SP_term_ref tObj, SP_term_ref tGoal, SP_term_ref tFlags, SP_term_ref tAnswer)
{
  SP_term_ref tResult = tAnswer; /* arbiitrary valid type value */
  int have_tResult = 0;
  return SPInvokeEx_helper(tObj, tGoal, tFlags, tAnswer, tResult, have_tResult);
}

static HRESULT SPInvokeEx(SP_term_ref tObj, SP_term_ref tGoal, SP_term_ref tFlags, SP_term_ref tAnswer, SP_term_ref tResult)
{
  int have_tResult = 0;
  return SPInvokeEx_helper(tObj, tGoal, tFlags, tAnswer, tResult, have_tResult);
}

#if 0                           /* [PM] 4.1.3 not used (for a SP_integer time) */
void SPCDECL comclient_invoke(SP_term_ref tObj, SP_term_ref tGoal, SP_term_ref tFlags, SP_term_ref tAnswer, SP_integer *result)
{
  HRESULT hr;

  EntryBreakPoint("comclient_invoke");

  hr = SPInvoke(tObj, tGoal, tFlags, tAnswer);
  /* TODO CLEANUP IF hr != S_OK */
  // if (FAILED(hr)) SP_raise_exception("comclient_create_instance");
  *result = hr;
}
#endif  /* 0 */

void SPCDECL comclient_invoke_ex(SP_term_ref tObj, SP_term_ref tGoal, SP_term_ref tFlags, SP_term_ref tAnswer, SP_term_ref tResult, SP_integer *result)
{
  HRESULT hr;

  EntryBreakPoint("comclient_invoke");

  hr = SPInvokeEx(tObj, tGoal, tFlags, tAnswer, tResult);
  /* TODO CLEANUP IF hr != S_OK */
  // if (FAILED(hr)) SP_raise_exception("comclient_create_instance");
  *result = hr;
}


void SPCDECL comclient_equal(SP_term_ref tObj1, SP_term_ref tObj2, SP_integer *pResult)
{
  IUnknown *pUnkn1a = NULL, *pUnkn1b = NULL, *pUnkn2a = NULL, *pUnkn2b = NULL;
  int result = 0;
  SP_integer tmp;
  
  pUnkn1a = comclient_get_interface(tObj1);
  if (!pUnkn1a) goto cleanup;
  pUnkn2a = comclient_get_interface(tObj2);
  if (!pUnkn2a) goto cleanup;
  if (pUnkn1a == pUnkn2a)
    {
      result = 1;
      goto cleanup;
    }
  
  if (FAILED(IUnknown_QueryInterface(pUnkn1a, &IID_IUnknown, &pUnkn1b))) goto cleanup;
  if (FAILED(IUnknown_QueryInterface(pUnkn2a, &IID_IUnknown, &pUnkn2b))) goto cleanup;
  result = (pUnkn1b == pUnkn2b);
  
 cleanup:
  if (pUnkn1a)
    {
      tmp = IUnknown_Release(pUnkn1a);
#if COMCLIENT_DEBUG
      fprintf(stderr, "DBG COMCLIENT: IUnknown_Release(0x%" SPRIxINTEGER ") == %" SPRIdINTEGER "\n", (SP_uinteger) pUnkn1a, (SP_integer) tmp);fflush(stderr);
#endif
    }
  if (pUnkn1b)
    {
      tmp = IUnknown_Release(pUnkn1b);
#if COMCLIENT_DEBUG
      fprintf(stderr, "DBG COMCLIENT: IUnknown_Release(0x%" SPRIxINTEGER ") == %" SPRIdINTEGER "\n", (SP_uinteger) pUnkn1b, (SP_integer) tmp);fflush(stderr);
#endif
    }
  if (pUnkn2a)
    {
      tmp = IUnknown_Release(pUnkn2a);
#if COMCLIENT_DEBUG
      fprintf(stderr, "DBG COMCLIENT: IUnknown_Release(0x%" SPRIxINTEGER ") == %" SPRIdINTEGER "\n", (SP_uinteger) pUnkn2a, (SP_integer) tmp);fflush(stderr);
#endif
    }
  if (pUnkn2b)
    {
      tmp = IUnknown_Release(pUnkn2b);
#if COMCLIENT_DEBUG
      fprintf(stderr, "DBG COMCLIENT: IUnknown_Release(0x%" SPRIxINTEGER ") == %" SPRIdINTEGER "\n", (SP_uinteger) pUnkn2b, (SP_integer) tmp);fflush(stderr);
#endif
    }
  *pResult = result;
}


void SPCDECL comclient_valid(SP_term_ref tObj, SP_integer *pResult)
{
  IUnknown *pUnkn = NULL;
  int result = 0;
  
  pUnkn = comclient_get_interface(tObj);
  result = !! pUnkn;
  
  if (pUnkn)
    {
      SP_integer tmp;
      tmp = IUnknown_Release(pUnkn);
#if COMCLIENT_DEBUG
      fprintf(stderr, "DBG COMCLIENT: IUnknown_Release(0x%" SPRIxINTEGER ") == %" SPRIdINTEGER "\n", (SP_uinteger) pUnkn, (SP_integer) tmp);fflush(stderr);
#endif
    }
  
  *pResult = result;
}

SP_integer SPCDECL comclient_garbage_collect(void)
{
  return SP_garbage_collect_external_objects(external_object_type__interface);
}



// for now
void SPCDECL comclient_init(int when)
{
  HRESULT hr;

  EntryBreakPoint("comclient_init");
  if (!coinitialize_called)
    {
      // hr = CoInitialize(NULL);
#if !WINCE
      hr = OleInitialize(NULL);
#else  /* WINCE */
#error "[PM] 3.11.2 COINIT_MULTITHREADED cannot be right for a sicstus library!"
      hr = CoInitializeEx( NULL, COINIT_MULTITHREADED);
#endif  /* WINCE */
      coinitialize_called = SUCCEEDED(hr);
    }
  SP_register_atom(atom_comclient_error = SP_atom_from_string("comclient_error"));
  SP_register_atom(atom_comclient_object = SP_atom_from_string("$comclient_object"));

  external_object_type__interface = SP_register_external_object_type(sp_interface_finalizer, comclient_interface_putter, NULL);
}

// for now
void SPCDECL comclient_deinit(int when)
{
  EntryBreakPoint("comclient_deinit");

  if (coinitialize_called)
    {
      coinitialize_called = 0;

#if !WINCE
      OleUninitialize();
#else  /* WINCE */
      CoUninitialize();
#endif  /* WINCE */
    }

#if 0
  external_object_deinit(when);
#endif
}





#if 0
void SPCDECL comclient_main(int argc, char* argv[])
{
  /* 0344 == small a diaresis */
  LPCWSTR s1 = L"H\344j D\344r!";
  char s2[50];
  char *s3;
  int rc = 42;
  BSTR bstr = NULL;
  IID iid;
  char *iname = "Nothing";
  
  printf("Hello World!\n");

  rc = INameToIID ("IDispatch", &iid);
  if (!SUCCEEDED(rc))
    {
      return -1;
    }

  rc = IIDToName (&iid, &iname);
  if (!SUCCEEDED(rc))
    {
      return -1;
    }

  rc = WideCharToMultiByte(
                           CP_UTF8, // UINT CodePage,            // code page
                           0, // DWORD dwFlags,            // performance and mapping flags
                           s1, // LPCWSTR lpWideCharStr,    // wide-character string
                           -1, // int cchWideChar,          // number of chars in string
                           s2, // LPSTR lpMultiByteStr,     // buffer for new string
                           (sizeof s2), //int cbMultiByte,          // size of buffer
                           NULL, // LPCSTR lpDefaultChar,     // default for unmappable chars
                           NULL // LPBOOL lpUsedDefaultChar  // set when default char used
                           );
  if (!rc)
    {
      return -1;
    }
  

  rc = SysAllocStringSP(s2, &bstr);
  
  if (!rc)
    {
      return -1;
    }

  rc = SysAllocSPTmpString(bstr, &s3);
  if (!rc)
    {
      return -1;
    }
  else
    {
      s3 = FreeTmpString(s3);
    }

  {
    BSTR progID = SysAllocString(L"Word.Application");
    BSTR clsidBSTR = NULL;

    rc = SP_CLSIDStringFromProgID(progID, &clsidBSTR);
    if (!SUCCEEDED(rc))
      {
        return -1;
      }

    progID = NULL;
    rc = SP_ProgIDFromCLSIDString(clsidBSTR, &progID);
    if (!SUCCEEDED(rc))
      {
        return -1;
      }
  }  
  return 0;
}
  
#endif /* 0 */

/* This should go in library(external_object) or some such */

void SPCDECL finalize_external_object(SP_term_ref tObj, SP_integer *pexisted)
{
  SP_finalize_external_object(tObj, pexisted);
}

