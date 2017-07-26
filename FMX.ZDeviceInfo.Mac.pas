unit FMX.ZDeviceInfo.Mac;

interface

uses
  FMX.Platform, FMX.ZDeviceInfo,
  FMX.PhoneDialer_iOSfix,
  FMX.PhoneDialer;

type
  TZiOSDeviceInfo = class(TInterfacedObject, IZDeviceInfoService)
    { IZDeviceInfoService }
    function PlatformVer: string;
    function Architecture2: string;
    function Device: string;
    function MacAddress: string;
    function IPAddress: string;
    function MobileOperator: string;
    function IsIntel: Boolean;

    function IsNetConnected: Boolean;
    function NetworkConnectionType: TZNetworkConnectionType;
    function MobileDataType: TZMobileDataType;

    function IsGPSActive(HIGH_ACCURACY: Boolean = false): Boolean;

    function DeviceID: string;
  end;

procedure RegisterService;
procedure UnregisterService;

implementation

{ TZiOSDeviceInfo }

uses
  System.SysUtils,
  MacApi.Foundation,
  MacApi.ObjectiveC, Posix.Wchar, MacApi.CoreFoundation, MacApi.Dispatch, Posix.SysSocket, Posix.SysSysctl;

// ---------------------------------------------------------------------------------------------------------------------
const
  libc = '/usr/lib/libc.dylib';
function sysctlbyname(Name: MarshaledAString; oldp: pointer; oldlen: Psize_t; newp: pointer; newlen: size_t): integer;
  cdecl; external libc name _PU + 'sysctlbyname';

function sysctl(Name: PInteger; namelen: cardinal; oldp: pointer; oldlen: Psize_t; newp: pointer; newlen: size_t)
  : integer; cdecl; external libc name _PU + 'sysctl';

function if_nametoindex(ifname: MarshaledAString): cardinal; cdecl; external libc name _PU + 'if_nametoindex';

function GetSysInfoByName(typeSpecifier: string): string;
var
  Size: integer;
  AResult: TArray<Byte>;
begin
  sysctlbyname(MarshaledAString(TMarshal.AsAnsi(typeSpecifier)), nil, @Size, nil, 0);
  SetLength(AResult, Size);
  sysctlbyname(MarshaledAString(TMarshal.AsAnsi(typeSpecifier)), MarshaledAString(AResult), @Size, nil, 0);
  Result := TEncoding.UTF8.GetString(AResult);
end;

(*
  function GetMacAddress: string;

  type
  sockaddr_dl = record
  sdl_len: char;
  sdl_family: char;
  sdl_index: word;
  sdl_type: char;
  sdl_nlen: char;
  sdl_alen: char;
  sdl_slen: char;
  sdl_data: string[12];
  end;

  type
  if_data = record
  end;

  type
  if_msghdr = record
  ifm_msglen: word;
  ifm_version: char;
  ifm_type: char;
  ifm_addrs: integer;
  ifm_flags: integer;
  ifm_index: word;
  ifm_data: if_data;
  end;

  const
  NET_RT_IFLIST = 3;

  var
  mib: array [0 .. 5] of integer;
  length: size_t;
  data: NSMutableData;
  socketAddress: sockaddr_dl;
  // if_msghdr: if_msghdr;
  coreAddress: char;
  begin
  mib[0] := CTL_NET;
  mib[1] := AF_ROUTE;
  mib[2] := 0;
  mib[3] := AF_LINK;
  mib[4] := NET_RT_IFLIST;
  mib[5] := if_nametoindex(MarshaledAString(TMarshal.AsAnsi('en0')));

  // get message size
  length = 0;
  if (mib[5] = 0) or (sysctl(mib, length(mib), nil, @length, nil, 0) < 0) or (length = 0) then
  exit('unknown');

  // get message
  data = TNSMutableData.Wrap(TNSMutableData.OCClass.dataWithLength(length));
  if (sysctl(mib, length(mib), data.mutableBytes, @length, nil, 0) < 0) then
  exit('unknown');

  // get socket address
  // socketAddress = data.mutableBytes + sizeof(if_msghdr);
  // coreAddress = LLADDR(socketAddress);
  // MacAddress = [[NSString alloc] initWithFormat: @" % 02 X: % 02 X: % 02 X: % 02 X: % 02 X: % 02 X ", coreAddress[0],
  // coreAddress[1], coreAddress[2], coreAddress[3], coreAddress[4], coreAddress[5]];
  end;
*)
// ---------------------------------------------------------------------------------------------------------------------

function TZiOSDeviceInfo.Architecture2: string;
begin
  Result := '';
end;

function TZiOSDeviceInfo.Device: string;
begin
  Result := 'MacOS (' + trim(GetSysInfoByName('hw.model')) + ')';
end;

function TZiOSDeviceInfo.DeviceID: string;
begin
  Result := '';
end;

function TZiOSDeviceInfo.IPAddress: string;
begin
  Result := 'unknown';
end;

function TZiOSDeviceInfo.IsGPSActive(HIGH_ACCURACY: Boolean): Boolean;
begin
  Result := false;
end;

function TZiOSDeviceInfo.IsIntel: Boolean;
begin
  Result := TOSVersion.Architecture in [arIntelX86, arIntelX64];
end;

function TZiOSDeviceInfo.IsNetConnected: Boolean;
begin
  Result := false;
end;

function TZiOSDeviceInfo.MacAddress: string;
begin
  Result := 'unknown';
end;

function TZiOSDeviceInfo.MobileOperator: string;
begin
  Result := 'not support';
end;

function TZiOSDeviceInfo.NetworkConnectionType: TZNetworkConnectionType;
begin
  Result := TZNetworkConnectionType.Unknown;
end;

function TZiOSDeviceInfo.MobileDataType: TZMobileDataType;
begin
  Result := TZMobileDataType.None;
end;

function TZiOSDeviceInfo.PlatformVer: string;
begin
  Result := GetSysInfoByName('kern.ostype') + ' ' + GetSysInfoByName('kern.osrelease');
end;

procedure RegisterService;
begin
  TPlatformServices.Current.AddPlatformService(IZDeviceInfoService, TZiOSDeviceInfo.Create);
end;

procedure UnregisterService;
begin
  TPlatformServices.Current.RemovePlatformService(IZDeviceInfoService);
end;

end.
