unit FMX.ZDeviceInfo.iOS;

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
  System.NetEncoding,
  iOSapi.CocoaTypes, iOSapi.Foundation, iOSapi.UIKit, FMX.Helpers.iOS;

function TZiOSDeviceInfo.Architecture2: string;
begin
  Result := '';
end;

function TZiOSDeviceInfo.Device: string;
begin
  with TUIDevice.Wrap(TUIDevice.OCClass.currentDevice) do
    Result := model.UTF8String;
end;

function TZiOSDeviceInfo.DeviceID: string;
begin
  with TUIDevice.Wrap(TUIDevice.OCClass.currentDevice) do
    Result := AnsiLowerCase(TNetEncoding.Base64.Encode(identifierForVendor.UUIDString.UTF8String));
end;

function TZiOSDeviceInfo.IPAddress: string;
begin
  Result := 'unknown';
end;

function TZiOSDeviceInfo.IsGPSActive(HIGH_ACCURACY: Boolean): Boolean;
begin
  Result := false; // ???
end;

function TZiOSDeviceInfo.IsIntel: Boolean;
begin
  Result := TOSVersion.Architecture in [arIntelX86, arIntelX64];
end;

function TZiOSDeviceInfo.IsNetConnected: Boolean;
begin
  Result := true; // ???
end;

function TZiOSDeviceInfo.MacAddress: string;
begin
  Result := '02:00:00:00:00:00';
end;

function TZiOSDeviceInfo.MobileOperator: string;
var
  aPhoneService: IFMXPhoneDialerService;
begin
  Result := 'unknown';
{$IFDEF CPUARM}
  if TPlatformServices.Current.SupportsPlatformService(IFMXPhoneDialerService, IInterface(aPhoneService)) then
    Result := aPhoneService.GetCarrier.GetCarrierName + ' ' + aPhoneService.GetCarrier.GetMobileCountryCode;
{$ENDIF}
end;

function TZiOSDeviceInfo.NetworkConnectionType: TZNetworkConnectionType;
begin
  Result := TZNetworkConnectionType.Unknown;
end;

function TZiOSDeviceInfo.MobileDataType: TZMobileDataType;
begin
  Result := TZMobileDataType.Unknown;
end;

function TZiOSDeviceInfo.PlatformVer: string;
begin
  with TUIDevice.Wrap(TUIDevice.OCClass.currentDevice) do
  begin
    Result := systemName.UTF8String + ' (' + systemVersion.UTF8String + ')';
  end;
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
