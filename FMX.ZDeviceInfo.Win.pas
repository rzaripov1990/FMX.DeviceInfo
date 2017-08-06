unit FMX.ZDeviceInfo.Win;

interface

uses
  FMX.Platform, FMX.ZDeviceInfo;

type
  TZWindowsDeviceInfo = class(TInterfacedObject, IZDeviceInfoService)
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

{ TZWindowsDeviceInfo }

uses
  System.SysUtils,
  System.NetEncoding,
  System.Variants,
  Winapi.Windows,
  Winapi.ActiveX,
  System.Win.ComObj;

// ---------------------------------------------------------------------------------------------------------------------
function InternetGetConnectedState(lpdwFlags: LPDWORD; dwReserved: DWORD): BOOL; stdcall;
  external 'wininet.dll' name 'InternetGetConnectedState';

procedure GetAddress(out aMac, aIP: string);
const
  wbemFlagForwardOnly = $00000020;
var
  FSWbemLocator, FWMIService, FWbemObjectSet, FWbemObject: OLEVariant;
  oEnum: IEnumvariant;
  iValue: LongWord;
begin
  FSWbemLocator := CreateOleObject('WbemScripting.SWbemLocator');
  FWMIService := FSWbemLocator.ConnectServer('localhost', 'root\CIMV2', '', '');
  FWbemObjectSet := FWMIService.ExecQuery
    ('SELECT Description,MACAddress,IPAddress FROM Win32_NetworkAdapterConfiguration WHERE IPEnabled=TRUE', 'WQL',
    wbemFlagForwardOnly);

  oEnum := IUnknown(FWbemObjectSet._NewEnum) as IEnumvariant;
  while oEnum.Next(1, FWbemObject, iValue) = 0 do
  begin
    if not VarIsNull(FWbemObject.MacAddress) then
      aMac := VarToStr(FWbemObject.MacAddress);
    if not VarIsNull(FWbemObject.IPAddress) then
      aIP := VarToStr(FWbemObject.IPAddress[0]);

    if not(aMac.IsEmpty and aIP.IsEmpty) then
    begin
      FWbemObject := Unassigned;
      break;
    end;
  end;
end;

function WinBuildNumber: string;
var
  FSWbemLocator, FWMIService, FWbemObjectSet, FWbemObject: OLEVariant;
  oEnum: IEnumvariant;
  iValue: LongWord;
begin
  FSWbemLocator := CreateOleObject('WbemScripting.SWbemLocator');
  FWMIService := FSWbemLocator.ConnectServer('localhost', 'root\CIMV2', '', '');
  FWbemObjectSet := FWMIService.ExecQuery('SELECT BuildNumber FROM Win32_OperatingSystem', 'WQL', 0);

  oEnum := IUnknown(FWbemObjectSet._NewEnum) as IEnumvariant;
  while oEnum.Next(1, FWbemObject, iValue) = 0 do
  begin
    Result := VarToStr(FWbemObject.BuildNumber);
    FWbemObject := Unassigned;
    break;
  end;
end;
// ---------------------------------------------------------------------------------------------------------------------

function TZWindowsDeviceInfo.Architecture2: string;
begin
  Result := '';
end;

function TZWindowsDeviceInfo.Device: string;
begin
  Result := TOSVersion.Name;
end;

function TZWindowsDeviceInfo.DeviceID: string;
var
  aMac, aIP: string;
begin
  try
    GetAddress(aMac, aIP);
    Result := AnsiLowerCase(TNetEncoding.Base64.Encode(aMac));
  except
    Result := '';
  end;
end;

function TZWindowsDeviceInfo.IPAddress: string;
var
  aMac: string;
begin
  try
    GetAddress(aMac, Result);
  except
    Result := 'unknown';
  end;
end;

function TZWindowsDeviceInfo.IsGPSActive(HIGH_ACCURACY: Boolean): Boolean;
begin
  Result := false;
end;

function TZWindowsDeviceInfo.IsIntel: Boolean;
begin
  Result := TOSVersion.Architecture in [arIntelX86, arIntelX64];
end;

function TZWindowsDeviceInfo.IsNetConnected: Boolean;
const
  INTERNET_CONNECTION_MODEM = 1;
  INTERNET_CONNECTION_LAN = 2;
  INTERNET_CONNECTION_PROXY = 4;
  // INTERNET_CONNECTION_MODEM_BUSY = 8;
var
  dwConnectionTypes: DWORD;
begin
  dwConnectionTypes := INTERNET_CONNECTION_MODEM or INTERNET_CONNECTION_LAN or INTERNET_CONNECTION_PROXY;
  Result := InternetGetConnectedState(@dwConnectionTypes, 0);
end;

function TZWindowsDeviceInfo.MacAddress: string;
var
  aIP: string;
begin
  try
    GetAddress(Result, aIP);
  except
    Result := '02:00:00:00:00:00';
  end;
end;

function TZWindowsDeviceInfo.MobileOperator: string;
begin
  Result := 'not support';
end;

function TZWindowsDeviceInfo.NetworkConnectionType: TZNetworkConnectionType;
begin
  Result := TZNetworkConnectionType.None;
  if IsNetConnected then
    Result := TZNetworkConnectionType.Ethernet; // ???
end;

function TZWindowsDeviceInfo.MobileDataType: TZMobileDataType;
begin
  Result := TZMobileDataType.None;
end;

function TZWindowsDeviceInfo.PlatformVer: string;
begin
  Result := TOSVersion.Major.ToString + '.' + TOSVersion.Minor.ToString + ' build ' + WinBuildNumber;
  // + TOSVersion.Build.ToString;
end;

procedure RegisterService;
begin
  TPlatformServices.Current.AddPlatformService(IZDeviceInfoService, TZWindowsDeviceInfo.Create);
end;

procedure UnregisterService;
begin
  TPlatformServices.Current.RemovePlatformService(IZDeviceInfoService);
end;

initialization

CoInitialize(nil);

finalization

CoUninitialize;

end.
