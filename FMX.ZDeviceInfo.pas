unit FMX.ZDeviceInfo;

{$SCOPEDENUMS ON}

interface

uses
  System.SysUtils, System.DateUtils, FMX.Platform;

type
  TZNetworkConnectionType = (None, Unknown, WIFI, Mobile, Ethernet);
  TZMobileDataType = (None, Unknown, n2G, n3G, n4G);

  IZDeviceInfoService = interface
    ['{AA275B1D-28A9-421B-BB3D-A86864A633A0}']
    function PlatformVer: string;
    function Architecture2: string;
    function Device: string;
    function MacAddress: string;
    function IPAddress: string;
    function MobileOperator: string;
    function IsIntel: boolean;

    function IsNetConnected: boolean;
    function NetworkConnectionType: TZNetworkConnectionType;
    function MobileDataType: TZMobileDataType;

    function IsGPSActive(HIGH_ACCURACY: boolean = false): boolean;

    function DeviceID: string;
  end;

  TZCustomDeviceInfo = class
  private const
    sPlatform: array [TOSVersion.TPlatform] of string = ('Windows', 'MacOS', 'iOS', 'Android', 'WinRT', 'Linux');
    sArchitecture: array [TOSVersion.TArchitecture] of string = ('IntelX86', 'IntelX64', 'ARM32', 'ARM64');
  private
    FScreenService: IFMXScreenService;
    FLocaleService: IFMXLocaleService;
    FDeviceInfoService: IZDeviceInfoService;
    FTimeZone: TTimeZone;
  public
    constructor Create; overload;
    destructor Destroy; override;

    function &Platform: string;
    function PlatformT: TOSVersion.TPlatform;
    function PlatformVer: string;
    function Architecture: string;
    function Architecture2: string;
    function ArchitectureT: TOSVersion.TArchitecture;
    function Device: string;
    function MacAddress: string;
    function IPAddress: string;
    function LangID: string;
    function ScreenPhis: string;
    function ScreenLogic: string;
    function ScreenWidth: Single;
    function ScreenHeight: Single;
    function Scale: Single;
    function MobileOperator: string;
    function TimeZone: integer;
    function IsIntel: boolean;

    function IsNetConnected: boolean;
    function NetworkConnectionType: TZNetworkConnectionType;
    function MobileDataType: TZMobileDataType;

    function IsPortraitOrientation: boolean;

    function IsGPSActive(HIGH_ACCURACY: boolean = false): boolean;

    function DeviceID: string;
  end;

  TZDeviceInfo = class(TZCustomDeviceInfo);

const
  TZNetworkConnectionTypeString: array [TZNetworkConnectionType] of string = ('None', 'Unknown', 'WIFI', 'Mobile Data',
    'Ethernet');

  TZMobileDataTypeString: array [TZMobileDataType] of string = ('None', 'Unknown', '2G', '3G', '4G/LTE');

implementation

{ TZCustomDeviceInfo }

uses
  FMX.Types, System.TimeSpan
{$IFDEF MSWINDOWS}
    , FMX.ZDeviceInfo.Win
{$ENDIF}
{$IFDEF ANDROID}
    , FMX.ZDeviceInfo.Android
{$ENDIF}
{$IFDEF IOS}
    , FMX.ZDeviceInfo.iOS
{$ELSE}
{$IFDEF MACOS}
    , FMX.ZDeviceInfo.Mac
{$ENDIF}
{$ENDIF};

function FloatS(const aValue: Single): string;
var
  Buf: TFormatSettings;
begin
  Buf := FormatSettings;
  Buf.DecimalSeparator := '.';
  Result := FloatToStr(aValue, Buf);
end;

function TZCustomDeviceInfo.Architecture: string;
begin
  Result := sArchitecture[TOSVersion.Architecture];
end;

function TZCustomDeviceInfo.Architecture2: string;
begin
  Result := '';
  if FDeviceInfoService <> nil then
    Result := FDeviceInfoService.Architecture2;
end;

function TZCustomDeviceInfo.ArchitectureT: TOSVersion.TArchitecture;
begin
  Result := TOSVersion.Architecture;
end;

constructor TZCustomDeviceInfo.Create;
begin
  inherited Create;
  TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, FScreenService);
  TPlatformServices.Current.SupportsPlatformService(IFMXLocaleService, FLocaleService);
  TPlatformServices.Current.SupportsPlatformService(IZDeviceInfoService, FDeviceInfoService);

{$WARNINGS OFF}
  FTimeZone := TTimeZone.Create;
{$WARNINGS ON}
end;

destructor TZCustomDeviceInfo.Destroy;
begin
  FScreenService := nil;
  FLocaleService := nil;
  FDeviceInfoService := nil;

  FTimeZone.Free;
  inherited;
end;

function TZCustomDeviceInfo.Device: string;
begin
  Result := '';
  if FDeviceInfoService <> nil then
    Result := FDeviceInfoService.Device;
end;

function TZCustomDeviceInfo.DeviceID: string;
begin
  Result := '';
  if FDeviceInfoService <> nil then
    Result := FDeviceInfoService.DeviceID;
end;

function TZCustomDeviceInfo.IPAddress: string;
begin
  Result := '';
  if FDeviceInfoService <> nil then
    Result := FDeviceInfoService.IPAddress;
end;

function TZCustomDeviceInfo.IsGPSActive(HIGH_ACCURACY: boolean): boolean;
begin
  Result := false;
  if FDeviceInfoService <> nil then
    Result := FDeviceInfoService.IsGPSActive(HIGH_ACCURACY);
end;

function TZCustomDeviceInfo.IsIntel: boolean;
begin
  Result := false;
  if FDeviceInfoService <> nil then
    Result := FDeviceInfoService.IsIntel;
end;

function TZCustomDeviceInfo.IsNetConnected: boolean;
begin
  Result := false;
  if FDeviceInfoService <> nil then
    Result := FDeviceInfoService.IsNetConnected;
end;

function TZCustomDeviceInfo.IsPortraitOrientation: boolean;
begin
  Result := true;
  if FScreenService <> nil then
    Result := (FScreenService.GetScreenOrientation = TScreenOrientation.Portrait) or
      (FScreenService.GetScreenOrientation = TScreenOrientation.InvertedPortrait);
end;

function TZCustomDeviceInfo.LangID: string;
begin
  Result := 'en';
  if FLocaleService <> nil then
    Result := FLocaleService.GetCurrentLangID;
end;

function TZCustomDeviceInfo.MacAddress: string;
begin
  Result := '';
  if FDeviceInfoService <> nil then
    Result := FDeviceInfoService.MacAddress;
end;

function TZCustomDeviceInfo.MobileOperator: string;
begin
  Result := '';
  if FDeviceInfoService <> nil then
    Result := FDeviceInfoService.MobileOperator;
end;

function TZCustomDeviceInfo.NetworkConnectionType: TZNetworkConnectionType;
begin
  Result := TZNetworkConnectionType.None;
  if FDeviceInfoService <> nil then
    Result := FDeviceInfoService.NetworkConnectionType;
end;

function TZCustomDeviceInfo.MobileDataType: TZMobileDataType;
begin
  Result := TZMobileDataType.None;
  if FDeviceInfoService <> nil then
    Result := FDeviceInfoService.MobileDataType;
end;

function TZCustomDeviceInfo.Platform: string;
begin
  Result := sPlatform[TOSVersion.Platform];
end;

function TZCustomDeviceInfo.PlatformT: TOSVersion.TPlatform;
begin
  Result := TOSVersion.Platform;
end;

function TZCustomDeviceInfo.PlatformVer: string;
begin
  Result := '';
  if FDeviceInfoService <> nil then
    Result := FDeviceInfoService.PlatformVer;
end;

function TZCustomDeviceInfo.Scale: Single;
begin
  Result := 1;
  if FScreenService <> nil then
    Result := FScreenService.GetScreenScale;
end;

function TZCustomDeviceInfo.ScreenWidth: Single;
begin
  Result := 0;
  if FScreenService <> nil then
    Result := FScreenService.GetScreenSize.X;
end;

function TZCustomDeviceInfo.ScreenHeight: Single;
begin
  Result := 0;
  if FScreenService <> nil then
    Result := FScreenService.GetScreenSize.Y;
end;

function TZCustomDeviceInfo.ScreenLogic: string;
begin
  Result := '0 x 0';
  if FScreenService <> nil then
    Result := FloatS(FScreenService.GetScreenSize.Round.X) + ' x ' + FloatS(FScreenService.GetScreenSize.Round.Y);
end;

function TZCustomDeviceInfo.ScreenPhis: string;
begin
  Result := '0 x 0';
  if FScreenService <> nil then
    Result := FloatS(FScreenService.GetScreenSize.Round.X * FScreenService.GetScreenScale) + ' x ' +
      FloatS(FScreenService.GetScreenSize.Round.Y * FScreenService.GetScreenScale);
end;

function TZCustomDeviceInfo.TimeZone: integer;
begin
{$WARNINGS OFF}
  Result := (((FTimeZone.Local.UtcOffset.Hours * 60) + FTimeZone.Local.UtcOffset.Minutes) * 60) +
    FTimeZone.Local.UtcOffset.Seconds;
{$WARNINGS ON}
end;

initialization

RegisterService;

finalization

UnregisterService;

end.
