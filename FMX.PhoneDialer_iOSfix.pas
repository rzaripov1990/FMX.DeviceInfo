unit FMX.PhoneDialer_iOSfix;

interface

const
  libCoreTelephony = '/System/Library/Frameworks/CoreTelephony.framework/CoreTelephony';

implementation

{$IF CompilerVersion < 32.0}

uses
  Posix.Dlfcn;

var
  CoreTelephonyModule: THandle;

initialization

CoreTelephonyModule := dlopen(MarshaledAString(libCoreTelephony), RTLD_LAZY);

finalization

dlclose(CoreTelephonyModule);
{$ENDIF}

end.
