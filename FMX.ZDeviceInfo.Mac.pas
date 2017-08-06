unit FMX.ZDeviceInfo.Mac;

interface

uses
  FMX.Platform, FMX.ZDeviceInfo;

type
  TZMacDeviceInfo = class(TInterfacedObject, IZDeviceInfoService)
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

{ TZMacDeviceInfo }

uses
  System.SysUtils,
  System.NetEncoding,
  MacApi.Foundation,
  MacApi.ObjectiveC,
  MacApi.Dispatch,
  MacApi.CoreFoundation,
  Posix.Wchar, Posix.SysSocket, Posix.SysSysctl,
  Posix.Base, Posix.NetIf,
  Posix.NetinetIn, Posix.ArpaInet;

type
  u_char = UInt8;
  u_short = UInt16;

  sockaddr_dl = record
    sdl_len: u_char; // * Total length of sockaddr */
    sdl_family: u_char; // * AF_LINK */
    sdl_index: u_short; // * if != 0, system given index for interface */
    sdl_type: u_char; // * interface type */
    sdl_nlen: u_char; // * interface name length, no trailing 0 reqd. */
    sdl_alen: u_char; // * link level address length */
    sdl_slen: u_char; // * link layer selector length */
    sdl_data: array [0 .. 11] of AnsiChar; // * minimum work area, can be larger;
    // contains both if name and ll address */
  end;

  psockaddr_dl = ^sockaddr_dl;

const
  IFT_ETHER = $6; // if_types.h

  // ---------------------------------------------------------------------------------------------------------------------
function sysctlbyname(Name: MarshaledAString; oldp: pointer; oldlen: Psize_t; newp: pointer; newlen: size_t): integer;
  cdecl; external libc name _PU + 'sysctlbyname';
function sysctl(Name: PInteger; namelen: cardinal; oldp: pointer; oldlen: Psize_t; newp: pointer; newlen: size_t)
  : integer; cdecl; external libc name _PU + 'sysctl';

function getifaddrs(var ifap: pifaddrs): integer; cdecl; external libc name _PU + 'getifaddrs';
{$EXTERNALSYM getifaddrs}
procedure freeifaddrs(ifp: pifaddrs); cdecl; external libc name _PU + 'freeifaddrs';
{$EXTERNALSYM freeifaddrs}
// function if_nametoindex(ifname: MarshaledAString): cardinal; cdecl; external libc name _PU + 'if_nametoindex';

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

function convertor(ip: integer): string;
begin
  Result := Format('%d.%d.%d.%d', [ip and $FF, ip shr 8 and $FF, ip shr 16 and $FF, ip shr 24 and $FF])
end;

function GetIPAddress: string;
var
  ifap, next: pifaddrs;
  sip4: Psockaddr_in;
  sip6: sockaddr_in6;
  MacAddr: array [0 .. 5] of Byte;
  I: integer;
begin
  Result := '';
  try
    if getifaddrs(ifap) = 0 then
    begin
      try
        next := ifap;
        while next <> nil do
        begin
          case next.ifa_addr.sa_family of
            AF_INET:
              begin
                // sip4 := @next;
                // Result := convertor(sip4.sin_addr.s_addr);
                break;
              end;
            AF_INET6:
              begin
                // sip6 := @next;
                // Result := (sip6.sin6_addr.s_addr);
                break;
              end;
          end;
          next := next.ifa_next;
        end;
      finally
        freeifaddrs(ifap);
      end;
    end;
    if Result = '' then
      Result := 'unknown';
  except
    Result := 'unknown';
  end;
end;

function GetMacAddress: string;
var
  ifap, next: pifaddrs;
  sdp: psockaddr_dl;
  MacAddr: array [0 .. 5] of Byte;
begin
  Result := '';
  try
    if getifaddrs(ifap) = 0 then
    begin
      try
        next := ifap;
        while next <> nil do
        begin
          case next.ifa_addr.sa_family of
            AF_LINK:
              begin
                sdp := psockaddr_dl(next.ifa_addr);
                if sdp.sdl_type = IFT_ETHER then
                begin
                  Move(pointer(PAnsiChar(@sdp^.sdl_data[0]) + sdp.sdl_nlen)^, MacAddr, 6);
                  Result := Result + IntToHex(MacAddr[0], 2) + ':' + IntToHex(MacAddr[1], 2) + ':' +
                    IntToHex(MacAddr[2], 2) + ':' + IntToHex(MacAddr[3], 2) + ':' + IntToHex(MacAddr[4], 2) + ':' +
                    IntToHex(MacAddr[5], 2) + ',';
                  break;
                end;
              end;
          end;
          next := next.ifa_next;
        end;
      finally
        freeifaddrs(ifap);
      end;
    end;
    if Result = '' then
      Result := '02:00:00:00:00:00';
  except
    Result := '02:00:00:00:00:00';
  end;
end;
// ---------------------------------------------------------------------------------------------------------------------

function TZMacDeviceInfo.Architecture2: string;
begin
  Result := '';
end;

function TZMacDeviceInfo.Device: string;
begin
  Result := 'MacOS (' + Trim(GetSysInfoByName('hw.model')) + ')';
end;

function TZMacDeviceInfo.DeviceID: string;
begin
  Result := AnsiLowerCase(TBase64Encoding.Create.Encode(GetMacAddress));
end;

function TZMacDeviceInfo.IPAddress: string;
begin
  Result := GetIPAddress;
end;

function TZMacDeviceInfo.IsGPSActive(HIGH_ACCURACY: Boolean): Boolean;
begin
  Result := false;
end;

function TZMacDeviceInfo.IsIntel: Boolean;
begin
  Result := TOSVersion.Architecture in [arIntelX86, arIntelX64];
end;

function TZMacDeviceInfo.IsNetConnected: Boolean;
begin
  Result := false;
end;

function TZMacDeviceInfo.MacAddress: string;
begin
  Result := GetMacAddress;
end;

function TZMacDeviceInfo.MobileOperator: string;
begin
  Result := 'not support';
end;

function TZMacDeviceInfo.NetworkConnectionType: TZNetworkConnectionType;
begin
  Result := TZNetworkConnectionType.Unknown;
end;

function TZMacDeviceInfo.MobileDataType: TZMobileDataType;
begin
  Result := TZMobileDataType.None;
end;

function TZMacDeviceInfo.PlatformVer: string;
begin
  Result := GetSysInfoByName('kern.ostype') + ' ' + GetSysInfoByName('kern.osrelease');
end;

procedure RegisterService;
begin
  TPlatformServices.Current.AddPlatformService(IZDeviceInfoService, TZMacDeviceInfo.Create);
end;

procedure UnregisterService;
begin
  TPlatformServices.Current.RemovePlatformService(IZDeviceInfoService);
end;

end.
