# DeviceInfo
# Device Info [Windows/Mac/IOS/Android]

#### Note:
#### ANDROID permissions:
####  --- access_network_state
####  --- acces_wifi_state

## Information
```
 TmyDeviceInfo = record
    diPlatform: string;
    diPlatformT: TOSVersion.TPlatform;
    diArchitecture: string;
    diArchitecture2: string;
    diArchitectureT: TOSVersion.TArchitecture;
    diMacAddress: string;
    diIPAddress: string;
    diPlatformVer: string;
    diDevice: string;
    diLang: string;
    diScreenPhis: string;
    diScreenLogic: string;
    diScreenWidth: Single;
    diScreenHeight: Single;
    diScale: Single;
    diMobileOperator: string;
    diTimeZone: integer;
    diIsIntel: Boolean;
  end;
```

## Functions
* function IsNetConnected: Boolean; - _check Internet connection [ANDROID, WINDOWS]_
* function IsNetConnectionType: TmyConnectionType; - _internet connection type [ANDROID, WINDOWS]_
* function IsNetworkType: TmyNetworkType; - _mobile network type [ANDROID]_
* function IsGPSActive(HIGH_ACCURACY: Boolean = False): Boolean; - _GPS enabled? [ANDROID]_
* function IsDeviceType: TDeviceInfo.TDeviceClass; - _device type (Unknown, Desktop, Phone, Tablet, etc) [ALL PLATFORMS]_
* function IsTablet: Boolean; - _the application is running on the tablet? [ALL PLATFORMS]_
* function IsPortraitOrientation: Boolean; - _current orientation is a Portrait? [ALL PLATFORMS]_
* function IsLargePhone: Boolean; - _Phablet? [ANDROID/IOS]_

![Android] (screenshots/android.png)
![Windows] (screenshots/win.png)
![IOS] (screenshots/ios.png)
![MAC OS] (screenshots/macos.png)
