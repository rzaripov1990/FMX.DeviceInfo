unit FMX.ZDeviceInfo.Android;

interface

uses
  FMX.Platform, FMX.ZDeviceInfo;

type
  TZAndroidDeviceInfo = class(TInterfacedObject, IZDeviceInfoService)
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

{ TZAndroidDeviceInfo }

uses
  System.SysUtils,
  System.NetEncoding,
  FMX.Types,
  AndroidApi.JNI.GraphicsContentViewText, AndroidApi.JNI.OS, AndroidApi.Helpers, AndroidApi.JNI.Net,
  AndroidApi.JNI.JavaTypes, AndroidApi.JNIBridge, AndroidApi.JNI.Provider, AndroidApi.JNI.Telephony,
  FMX.PhoneDialer, FMX.PhoneDialer.Android, FMX.Platform.Android,
  AndroidApi.JNI.Java.Net,
  AndroidApi.JNI.Android.Security;
// , AndroidApi.Log;

// ---------------------------------------------------------------------------------------------------------------------
function HasPermission(const Permission: string): Boolean;
var
  aPerm: string;
begin
  if Permission.StartsWith('android.permission.') then
    aPerm := Permission
  else
    aPerm := 'android.permission.' + Permission;
  Result := TAndroidHelper.Context.checkSelfPermission(StringToJString(aPerm))
    = TJPackageManager.JavaClass.PERMISSION_GRANTED
end;

function GetCodename(VerString: string): string;
begin
  if Pos('4.4', VerString) = 1 then
    Result := 'Kit Kat'
  else if Pos('4.0', VerString) > 0 then
    Result := 'ICS'
  else if Pos('4.', VerString) > 0 then
    Result := 'JB'
  else if (Pos('5.', VerString) > 0) then
    Result := 'Lollipop'
  else if Pos('6.', VerString) > 0 then
    Result := 'Marshmallow'
  else if Pos('7.', VerString) > 0 then
    Result := 'Nougat'
  else
    Result := 'Unknown';
end;

function GetWifiManager: JWifiManager;
var
  WifiManagerObj: JObject;
begin
  Result := nil;
  if HasPermission('ACCESS_WIFI_STATE') then
  begin
    WifiManagerObj := TAndroidHelper.Context.getSystemService(TJContext.JavaClass.WIFI_SERVICE);
    if not Assigned(WifiManagerObj) then
      raise Exception.Create('Could not locate Wifi Service');
    Result := TJWifiManager.Wrap((WifiManagerObj as ILocalObject).GetObjectID);
    if not Assigned(Result) then
      raise Exception.Create('Could not access Wifi Manager');
  end
  else
    Log.d(':: ZDeviceInfo :: permission ACCESS_WIFI_STATE denied');
end;

function GetTelephonyManager: JTelephonyManager;
var
  TelephoneServiceNative: JObject;
begin
  Result := nil;
  TelephoneServiceNative := TAndroidHelper.Context.getSystemService(TJContext.JavaClass.TELEPHONY_SERVICE);
  if not Assigned(TelephoneServiceNative) then
    raise Exception.Create('Could not locate Telephony Service');
  Result := TJTelephonyManager.Wrap((TelephoneServiceNative as ILocalObject).GetObjectID);
  if not Assigned(Result) then
    raise Exception.Create('Could not access Telephony Manager');
end;

function GetConnectivityManager: JConnectivityManager;
var
  ConnectivityServiceNative: JObject;
begin
  Result := nil;
  if HasPermission('ACCESS_NETWORK_STATE') then
  begin
    ConnectivityServiceNative := TAndroidHelper.Context.getSystemService(TJContext.JavaClass.CONNECTIVITY_SERVICE);
    if not Assigned(ConnectivityServiceNative) then
      raise Exception.Create('Could not locate Connectivity Service');
    Result := TJConnectivityManager.Wrap((ConnectivityServiceNative as ILocalObject).GetObjectID);
    if not Assigned(Result) then
      raise Exception.Create('Could not access Connectivity Manager');
  end
  else
    Log.d(':: ZDeviceInfo :: permission ACCESS_NETWORK_STATE denied');
end;

function convertor(ip: integer): string;
begin
  Result := Format('%d.%d.%d.%d', [ip and $FF, ip shr 8 and $FF, ip shr 16 and $FF, ip shr 24 and $FF])
end;

function getMobileType(jType: integer): TZNetworkConnectionType;
begin
  Result := TZNetworkConnectionType.Unknown; // Unknown connection type
  if jType = TJConnectivityManager.JavaClass.TYPE_ETHERNET then
    Result := TZNetworkConnectionType.Ethernet
  else if jType = TJConnectivityManager.JavaClass.TYPE_WIFI then
    Result := TZNetworkConnectionType.WIFI
  else
  begin
    if jType in [TJConnectivityManager.JavaClass.TYPE_MOBILE, TJConnectivityManager.JavaClass.TYPE_MOBILE_DUN,
      TJConnectivityManager.JavaClass.TYPE_MOBILE_HIPRI, TJConnectivityManager.JavaClass.TYPE_MOBILE_MMS,
      TJConnectivityManager.JavaClass.TYPE_MOBILE_SUPL, TJConnectivityManager.JavaClass.TYPE_WIMAX] then
      Result := TZNetworkConnectionType.Mobile;
  end;
end;

function getMobileSubType(jType: integer): TZMobileDataType;
begin
  Result := TZMobileDataType.Unknown;
  if jType in [TJTelephonyManager.JavaClass.NETWORK_TYPE_GPRS, TJTelephonyManager.JavaClass.NETWORK_TYPE_EDGE,
    TJTelephonyManager.JavaClass.NETWORK_TYPE_CDMA, TJTelephonyManager.JavaClass.NETWORK_TYPE_1xRTT,
    TJTelephonyManager.JavaClass.NETWORK_TYPE_IDEN] then
    Result := TZMobileDataType.n2G
  else if jType in [TJTelephonyManager.JavaClass.NETWORK_TYPE_UMTS, TJTelephonyManager.JavaClass.NETWORK_TYPE_EVDO_0,
    TJTelephonyManager.JavaClass.NETWORK_TYPE_EVDO_A, TJTelephonyManager.JavaClass.NETWORK_TYPE_HSDPA,
    TJTelephonyManager.JavaClass.NETWORK_TYPE_HSUPA, TJTelephonyManager.JavaClass.NETWORK_TYPE_HSPA,
    TJTelephonyManager.JavaClass.NETWORK_TYPE_EVDO_B, TJTelephonyManager.JavaClass.NETWORK_TYPE_EHRPD,
    TJTelephonyManager.JavaClass.NETWORK_TYPE_HSPAP] then
    Result := TZMobileDataType.n3G
  else if (jType = TJTelephonyManager.JavaClass.NETWORK_TYPE_LTE) then
    Result := TZMobileDataType.n4G;
end;

function GetConnectionType: TZNetworkConnectionType;
var
  ConnectivityManager: JConnectivityManager;
  ActiveNetwork: JNetworkInfo;
begin
  Result := TZNetworkConnectionType.None;
  ConnectivityManager := GetConnectivityManager;
  if ConnectivityManager = nil then
    exit;

  ActiveNetwork := ConnectivityManager.getActiveNetworkInfo;
  if Assigned(ActiveNetwork) and ActiveNetwork.isConnected then
    Result := getMobileType(ActiveNetwork.getType);
end;

{
  interface names might be handled differently on different devices.
  But in most cases you can tell from the IP where it belongs to.
  Furthermore, `rmnet` is mostly used for GPRS (mobile data), while WiFi uses names like `tiwlan` or `eth`
}
procedure GetAddress(out aMac, aWifiIP: string);
var
  WifiManager: JWifiManager;
  WifiInfo: JWifiInfo;

  // Intf: JNetworkInterface;
  // Adrs: JInetAddress;

  // AdrsEnum: JEnumeration;
begin
  aMac := '02:00:00:00:00:00';
  aWifiIP := 'unknown';
  WifiManager := GetWifiManager;
  if WifiManager = nil then
    exit;

  case GetConnectionType of
    TZNetworkConnectionType.WIFI:
      begin
        WifiInfo := WifiManager.getConnectionInfo;
        aMac := JStringToString(WifiInfo.getMacAddress);
        aWifiIP := convertor(WifiInfo.GetIPAddress);
      end;
    TZNetworkConnectionType.Mobile, TZNetworkConnectionType.Ethernet:
      begin
        // Intf := TJNetworkInterface.JavaClass.getByName(StringToJString('eth0'));
        // if Intf = nil then
        // Intf := TJNetworkInterface.JavaClass.getByName(StringToJString('wlan0'));
        // if Intf = nil then
        // exit;
        // Log.d(':: ZDeviceInfo :: interface :: ' + JStringToString(Intf.getDisplayName));

        // while (Intf <> nil) and (IntfEnum.hasMoreElements) do
        // begin
        // AdrsEnum := Intf.getInetAddresses;
        // if AdrsEnum.hasMoreElements then
        // begin
        // Adrs := TJInetAddress.Wrap((AdrsEnum.nextElement as ILocalObject).GetObjectID);
        // Log.d(':: ZDeviceInfo :: address :: ' + JStringToString(Intf.getDisplayName) + ' :: ' +
        // JStringToString(Adrs.getHostAddress));
        //
        // while (Adrs <> nil) do
        // begin
        // if (not Adrs.isLoopbackAddress) then
        // begin
        // Log.d(':: ZDeviceInfo :: address :: ' + JStringToString(Adrs.getHostAddress) + ' :: ' +
        // JStringToString(Adrs.getCanonicalHostName));
        // aWifiIP := JStringToString(Adrs.getHostAddress);
        // break;
        // end;
        //
        // Adrs := TJInetAddress.Wrap((AdrsEnum.nextElement as ILocalObject).GetObjectID);
        // Log.d(':: ZDeviceInfo :: address :: ' + JStringToString(Intf.getDisplayName) + ' :: ' +
        // JStringToString(Adrs.getHostName) + ' :: ' + JStringToString(Adrs.getHostAddress));
        // end;
        // end;
      end;
  end;

  (* exit;

    // Hot Spot #2
    if aWifiIP.Equals('0.0.0.0') then
    begin
    // IntfEnum := TJNetworkInterface.JavaClass.getNetworkInterfaces;

    Intf := TJNetworkInterface.JavaClass.getByName(StringToJString('wlan0'));
    // if IntfEnum.hasMoreElements then
    // begin
    // Intf := TJNetworkInterface.Wrap((IntfEnum.nextElement as ILocalObject).GetObjectID);
    Log.d(':: ZDeviceInfo :: first interface :: ' + JStringToString(Intf.getDisplayName) + ' :: ' +
    JStringToString(Intf.getName));

    // while (Intf <> nil) and (IntfEnum.hasMoreElements) do
    // begin
    AdrsEnum := Intf.getInetAddresses;
    if AdrsEnum.hasMoreElements then
    begin
    Adrs := TJInetAddress.Wrap((AdrsEnum.nextElement as ILocalObject).GetObjectID);
    Log.d(':: ZDeviceInfo :: first address :: ' + JStringToString(Intf.getDisplayName) + ' :: ' +
    JStringToString(Adrs.getHostName) + ' :: ' + JStringToString(Adrs.getHostAddress));

    while (Adrs <> nil) do
    begin
    if (not Adrs.isLoopbackAddress) then
    begin
    Log.d(':: ZDeviceInfo :: address :: ' + JStringToString(Adrs.getHostAddress) + ' :: ' +
    JStringToString(Adrs.getHostName) + ' :: ' + JStringToString(Adrs.getCanonicalHostName));
    aWifiIP := JStringToString(Adrs.getHostAddress);
    break;
    end;

    Adrs := TJInetAddress.Wrap((AdrsEnum.nextElement as ILocalObject).GetObjectID);
    // if not Adrs.isLoopbackAddress then
    Log.d(':: ZDeviceInfo :: address :: ' + JStringToString(Intf.getDisplayName) + ' :: ' +
    JStringToString(Adrs.getHostName) + ' :: ' + JStringToString(Adrs.getHostAddress));
    end;
    end;

    // Intf := TJNetworkInterface.Wrap((IntfEnum.nextElement as ILocalObject).GetObjectID);
    // if Intf <> nil then
    // Log.d(':: ZDeviceInfo :: interface :: ' + JStringToString(Intf.getDisplayName) + ' :: ' +
    // JStringToString(Intf.getName));
    // end;
    // end;
    end; *)
end;
// :: ZDeviceInfo :::: ZDeviceInfo :::: ZDeviceInfo :::: ZDeviceInfo :::: ZDeviceInfo :::: ZDeviceInfo :::: ZDeviceInfo :::: ZDeviceInfo :::: ZDeviceInfo :::: ZDeviceInfo :::: ZDeviceInfo ::-------

function TZAndroidDeviceInfo.Architecture2: string;
var
  JavaStrings: TJavaObjectArray<JString>;
  I: integer;
begin
  Result := '';
  if TOSVersion.Major >= 5 then
  begin
    JavaStrings := TJBuild.JavaClass.SUPPORTED_ABIS;
    for I := 0 to JavaStrings.Length - 1 do
      Result := Result + ',' + JStringToString(JavaStrings.Items[I]);
    Result := Result.trim([',']);
  end
  else
    Result := JStringToString(TJBuild.JavaClass.CPU_ABI) + ',' + JStringToString(TJBuild.JavaClass.CPU_ABI2);
end;

function TZAndroidDeviceInfo.Device: string;
begin
  Result := JStringToString(TJBuild.JavaClass.MANUFACTURER) + ' ' + JStringToString(TJBuild.JavaClass.model);
end;

function TZAndroidDeviceInfo.DeviceID: string;
begin
  Result := AnsiLowerCase(TNetEncoding.Base64.Encode(JStringToString(TJSettings_Secure.JavaClass.getString
    (TAndroidHelper.Context.getContentResolver, TJSettings_Secure.JavaClass.ANDROID_ID))));
end;

function TZAndroidDeviceInfo.IPAddress: string;
var
  aMac: string;
begin
  try
    GetAddress(aMac, Result);
  except
    Result := 'unknown';
  end;
end;

function TZAndroidDeviceInfo.IsGPSActive(HIGH_ACCURACY: Boolean): Boolean;
var
  Provider: string;
  LocationMode: integer;
begin
  Result := false;
  if TOSVersion.Check(4, 4) then
  begin
    LocationMode := TJSettings_Secure.JavaClass.getInt(TAndroidHelper.Context.getContentResolver,
      TJSettings_Secure.JavaClass.LOCATION_MODE);
    if HIGH_ACCURACY then
      Result := LocationMode <> TJSettings_Secure.JavaClass.LOCATION_MODE_HIGH_ACCURACY
    else
      Result := LocationMode <> TJSettings_Secure.JavaClass.LOCATION_MODE_OFF;
  end
  else
  begin
    Provider := JStringToString(TJSettings_Secure.JavaClass.getString(TAndroidHelper.Context.getContentResolver,
      TJSettings_system.JavaClass.LOCATION_PROVIDERS_ALLOWED));
    if HIGH_ACCURACY then
      Result := Pos('gps', Provider) > 0
    else
      Result := (Pos('network', Provider) > 0) or (Pos('gps', Provider) > 0);
  end;
end;

function TZAndroidDeviceInfo.IsIntel: Boolean;
begin
  Result := Architecture2.Contains('x86') or JStringToString(TJBuild.JavaClass.FINGERPRINT).Contains('intel');
end;

function TZAndroidDeviceInfo.IsNetConnected: Boolean;
begin
  Result := NetworkConnectionType <> TZNetworkConnectionType.None;
end;

function TZAndroidDeviceInfo.MacAddress: string;
var
  aIP: string;
begin
  Result := '02:00:00:00:00:00';
  if TOSVersion.Major <= 5 then
    try
      GetAddress(Result, aIP);
    except
      Result := '02:00:00:00:00:00';
    end;
end;

function TZAndroidDeviceInfo.MobileOperator: string;
var
  aPhoneService: IFMXPhoneDialerService;
begin
  Result := 'unknown';
  if TPlatformServices.Current.SupportsPlatformService(IFMXPhoneDialerService, IInterface(aPhoneService)) then
    Result := aPhoneService.GetCarrier.GetCarrierName + ' ' + aPhoneService.GetCarrier.GetMobileCountryCode;
end;

function TZAndroidDeviceInfo.NetworkConnectionType: TZNetworkConnectionType;
begin
  Result := GetConnectionType;
end;

function TZAndroidDeviceInfo.MobileDataType: TZMobileDataType;
var
  TelephoneManager: JTelephonyManager;
begin
  Result := TZMobileDataType.None;
  TelephoneManager := GetTelephonyManager;
  if (Assigned(TelephoneManager)) and (TelephoneManager.getSimState = TJTelephonyManager.JavaClass.SIM_STATE_READY) then
    Result := getMobileSubType(TelephoneManager.getNetworkType);
end;

function TZAndroidDeviceInfo.PlatformVer: string;
begin
  Result := GetCodename(JStringToString(TJBuild_VERSION.JavaClass.release)) + ' ' +
    JStringToString(TJBuild_VERSION.JavaClass.release)
end;

procedure RegisterService;
begin
  TPlatformServices.Current.AddPlatformService(IZDeviceInfoService, TZAndroidDeviceInfo.Create);
end;

procedure UnregisterService;
begin
  TPlatformServices.Current.RemovePlatformService(IZDeviceInfoService);
end;

end.
