.386
.MODEL FLAT, STDCALL
OPTION casemap:none

INCLUDE windows.inc
INCLUDE kernel32.inc
INCLUDELIB kernel32.lib
INCLUDE user32.inc
INCLUDELIB user32.lib
INCLUDE shell32.inc
INCLUDELIB shell32.lib

.CONST
filename_len		EQU 65536
cmdline_len		EQU 2*65536
error_unexpectederror	EQU 201
error_createprocess	EQU 202
error_getexitcode	EQU 203
ytdexe			DB 'ytd.exe', 0
firstquote		DB '"', 0
lastquote		DB '" ', 0
setupcmdline		DB '--setup', 0

.DATA

.DATA?
resultcode		DD ?
cmdline			DD ?
filename		DB filename_len DUP (?)
runcmdline		DB cmdline_len DUP (?)
startupinfo		STARTUPINFOA <>
processinfo		PROCESS_INFORMATION <>

.CODE

start:
	; Get process filename
	INVOKE GetModuleFileNameA, NULL, ADDR filename, filename_len
	OR eax, eax
	JE UnexpectedError

	; Generate filename of YTD.EXE
	LEA edi, filename
	MOV ecx, filename_len
	MOV al, 0
	REPNZ SCASB
	JNZ UnexpectedError
	DEC edi
	SUB ecx, filename_len
	NEG ecx
	MOV al, '\'
	STD
	REPNZ SCASB
	CLD
	JNZ nopath
	INC edi
nopath:
	INC edi
	MOV BYTE PTR [edi], 0
	INVOKE lstrcatA, ADDR filename, ADDR ytdexe

	; Get the command line (without filename)
	INVOKE GetCommandLineA
	MOV edi, eax
	LEA ebx, setupcmdline
	MOV cmdline, EBX
	OR eax, eax
	JE UnexpectedError
	CMP BYTE PTR [edi], 0
	JE nocmdline
	CMP BYTE PTR [edi], '"'
	JNE noquotes
	INC edi
	INVOKE lstrlenA, edi
	MOV ecx, eax
	MOV al, '"'
	REPNZ SCASB
	JNE UnexpectedError
noquotes:
	INVOKE lstrlenA, edi
	MOV ecx, eax
	MOV al, ' '
	REPNZ SCASB
	JNE nocmdline
	DEC edi
	INC ecx
	REPZ SCASB
	JE nocmdline
	DEC edi
	MOV cmdline, edi
nocmdline:

	; Get desired command line length
	MOV ecx, 3
	INVOKE lstrlenA, ADDR filename
	ADD ecx, eax
	INVOKE lstrlenA, [cmdline]
	ADD ecx, eax
	CMP eax, cmdline_len
	JAE UnexpectedError

	; Build the desired command line
	LEA edi, runcmdline
	MOV BYTE PTR [edi], 0
	INVOKE lstrcatA, edi, ADDR firstquote
	INVOKE lstrcatA, edi, ADDR filename
	INVOKE lstrcatA, edi, ADDR lastquote
	INVOKE lstrcatA, edi, [cmdline]

	; Prepare startup info
	LEA edi, startupinfo
	MOV ecx, LENGTH startupinfo
	MOV al, 0
	REP STOSB
	MOV startupinfo.cb, LENGTH startupinfo
	MOV startupinfo.dwFlags, STARTF_USESHOWWINDOW
	MOV startupinfo.wShowWindow, SW_SHOWNORMAL
	; Create the process
	INVOKE CreateProcessA, NULL, ADDR runcmdline, NULL, NULL, 0, NORMAL_PRIORITY_CLASS, NULL, NULL, ADDR startupinfo, ADDR processinfo
	OR eax, eax
	JE CreateProcessFailed
	; Wait for process to terminate
	INVOKE WaitForMultipleObjects, 1, ADDR processinfo.hProcess, 1, INFINITE
	; Get the result code
	INVOKE GetExitCodeProcess, [processinfo.hProcess], ADDR resultcode
	OR eax, eax
	JNE gotresultcode
	MOV resultcode, error_getexitcode
gotresultcode:
	; Close the handles
	INVOKE CloseHandle, [processinfo.hProcess]
	INVOKE CloseHandle, [processinfo.hThread]
	; Return
	INVOKE ExitProcess, [resultcode]

UnexpectedError:
	INVOKE ExitProcess, error_unexpectederror

CreateProcessFailed:
	INVOKE ExitProcess, error_createprocess

end start
