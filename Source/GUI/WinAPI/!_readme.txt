1) ResEdit requires that the resource file is called resource.h

2) BRCC32 incorrectly parses windows.h file. At the moment I know that I need
   these lines in resource.h:

#ifndef LVS_OWNERDATA
#define LVS_OWNERDATA 4096
#endif

