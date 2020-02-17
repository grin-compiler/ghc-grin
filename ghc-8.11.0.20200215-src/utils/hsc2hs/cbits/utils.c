/* ----------------------------------------------------------------------------
   (c) The University of Glasgow 2006, Lifted from Bases

   Useful Win32 bits
   ------------------------------------------------------------------------- */

#if defined(_WIN32)

#include "HsBase.h"
#include <stdbool.h>
#include <stdint.h>
/* Using Secure APIs */
#define MINGW_HAS_SECURE_API 1
#include <wchar.h>
#include <windows.h>

/* Copied from getTempFileNameErrorNo in base's cbits/Win32Utils.c in GHC 8.10.
   Check there for any bugfixes first and please keep in sync when making
   changes.  */

bool __get_temp_file_name (wchar_t* pathName, wchar_t* prefix,
                           wchar_t* suffix, uint32_t uUnique,
                           wchar_t* tempFileName)
{
  int retry = 5;
  bool success = false;
  while (retry > 0 && !success)
    {
      // TODO: This needs to handle long file names.
      if (!GetTempFileNameW(pathName, prefix, uUnique, tempFileName))
        {
          maperrno();
          return false;
        }

      wchar_t* drive = malloc (sizeof(wchar_t) * _MAX_DRIVE);
      wchar_t* dir   = malloc (sizeof(wchar_t) * _MAX_DIR);
      wchar_t* fname = malloc (sizeof(wchar_t) * _MAX_FNAME);
      if (_wsplitpath_s (tempFileName, drive, _MAX_DRIVE, dir, _MAX_DIR,
                         fname, _MAX_FNAME, NULL, 0) != 0)
        {
          success = false;
          maperrno ();
        }
      else
        {
          wchar_t* temp = _wcsdup (tempFileName);
          if (wcsnlen(drive, _MAX_DRIVE) == 0)
            swprintf_s(tempFileName, MAX_PATH, L"%s\%s%s",
                      dir, fname, suffix);
          else
            swprintf_s(tempFileName, MAX_PATH, L"%s\%s\%s%s",
                      drive, dir, fname, suffix);
          success
             = MoveFileExW(temp, tempFileName, MOVEFILE_WRITE_THROUGH
                                               | MOVEFILE_COPY_ALLOWED) != 0;
          errno = 0;
          if (!success && (GetLastError () != ERROR_FILE_EXISTS || --retry < 0))
            {
              success = false;
              maperrno ();
              DeleteFileW (temp);
            }


          free(temp);
        }

      free(drive);
      free(dir);
      free(fname);
    }

  return success;
}

typedef void(*setterDef)(DWORD, HANDLE);
typedef HANDLE(*getterDef)(DWORD);
typedef PHANDLE ProcHandle;

/* Copied from cbits/runProcess.c in version 1.6.6.1 of the process library
   Check there for any bugfixes first and please keep in sync when making
   changes.  Drop this when that version is the minimal supported process
   version.  */

static int
waitForProcess (ProcHandle handle, int *pret)
{
    DWORD retCode;

    if (WaitForSingleObject((HANDLE) handle, INFINITE) == WAIT_OBJECT_0)
    {
        if (GetExitCodeProcess((HANDLE) handle, &retCode) == 0)
        {
            maperrno();
            return -1;
        }
        *pret = retCode;
        return 0;
    }

    maperrno();
    return -1;
}


int
__waitForJobCompletion ( HANDLE hJob, HANDLE ioPort, DWORD timeout,
                         int *pExitCode, setterDef set, getterDef get )
{
    DWORD CompletionCode;
    ULONG_PTR CompletionKey;
    LPOVERLAPPED Overlapped;
    *pExitCode = 0;

    // We have to loop here. It's a blocking call, but
    // we get notified on each completion event. So if it's
    // not one we care for we should just block again.
    // If all processes are finished before this call is made
    // then the initial call will return false.
    // List of events we can listen to:
    // https://msdn.microsoft.com/en-us/library/windows/desktop/ms684141(v=vs.85).aspx
    while (GetQueuedCompletionStatus (ioPort, &CompletionCode,
                                      &CompletionKey, &Overlapped, timeout)) {

        // If event wasn't meant of us, keep listening.
        if ((HANDLE)CompletionKey != hJob)
          continue;

        switch (CompletionCode)
        {
            case JOB_OBJECT_MSG_NEW_PROCESS:
            {
                // A new child process is born.
                // Retrieve and save the process handle from the process id.
                // We'll need it for later but we can't retrieve it after the
                // process has exited.
                DWORD pid    = (DWORD)(uintptr_t)Overlapped;
                HANDLE pHwnd = OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION,
                                           TRUE, pid);
                set(pid, pHwnd);
            }
            break;
            case JOB_OBJECT_MSG_ABNORMAL_EXIT_PROCESS:
            case JOB_OBJECT_MSG_EXIT_PROCESS:
            {
                // A child process has just exited.
                // Read exit code, We assume the last process to exit
                // is the process whose exit code we're interested in.
                HANDLE pHwnd = get((DWORD)(uintptr_t)Overlapped);
                if (GetExitCodeProcess(pHwnd, (DWORD *)pExitCode) == 0)
                {
                    maperrno();
                    return 1;
                }

                // Check to see if the child has actually exited.
                if (*(DWORD *)pExitCode == STILL_ACTIVE)
                  waitForProcess ((ProcHandle)pHwnd, pExitCode);
            }
            break;
            case JOB_OBJECT_MSG_ACTIVE_PROCESS_ZERO:
                // All processes in the tree are done.
                return 0;
            default:
                break;
        }
    }

    // Check to see if a timeout has occurred or that the
    // all processes in the job were finished by the time we
    // got to the loop.
    if (Overlapped == NULL && (HANDLE)CompletionKey != hJob)
    {
        // Timeout occurred.
        return -1;
    }

    return 0;
}
#endif