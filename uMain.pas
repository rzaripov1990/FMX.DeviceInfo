unit uMain;

{
  Device Info

  author: ZuBy
  https://github.com/rzaripov1990/DeviceInfo
  rzaripov1990@gmail.com

  ANDROID permissions:
  ..access_network_state
  ..acces_wifi_state
}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ListBox, FMX.Layouts;

type
  TForm9 = class(TForm)
    ListBox1: TListBox;
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form9: TForm9;

implementation

{$R *.fmx}

uses
  FMX.DeviceInfo;

procedure TForm9.FormActivate(Sender: TObject);
var
  aItem: TListBoxItem;
begin
  if Tag = 0 then
    exit;
  Tag := 0;

  DeviceInfoByPlatform;

  with DeviceInfo do
  begin
    // device info
    aItem := TListBoxGroupHeader.Create(nil);
    aItem.Parent := ListBox1;
    aItem.Text := 'Device Info';

    aItem := TListBoxItem.Create(nil);
    aItem.Parent := ListBox1;
    aItem.ItemData.Detail := 'Device Name';
    aItem.Text := diDevice;

    aItem := TListBoxItem.Create(nil);
    aItem.Parent := ListBox1;
    aItem.ItemData.Detail := 'Platform';
    aItem.Text := diPlatform + ' ' + diPlatformVer;

    aItem := TListBoxItem.Create(nil);
    aItem.Parent := ListBox1;
    aItem.ItemData.Detail := 'Architecture';
    aItem.Text := diArchitecture + '/' + diArchitecture2;

    aItem := TListBoxItem.Create(nil);
    aItem.Parent := ListBox1;
    aItem.ItemData.Detail := 'Maybe Is Intel';
    aItem.Text := BoolToStr(diIsIntel, true);

    aItem := TListBoxItem.Create(nil);
    aItem.Parent := ListBox1;
    aItem.ItemData.Detail := 'Lang';
    aItem.Text := diLang;

    aItem := TListBoxItem.Create(nil);
    aItem.Parent := ListBox1;
    aItem.ItemData.Detail := 'TimeZone (in seconds)';
    aItem.Text := diTimeZone.ToString;

    aItem := TListBoxItem.Create(nil);
    aItem.Parent := ListBox1;
    aItem.ItemData.Detail := 'Orientation';
    if IsPortraitOrientation then
      aItem.Text := 'Portrait'
    else
      aItem.Text := 'Landscape';

    aItem := TListBoxItem.Create(nil);
    aItem.Parent := ListBox1;
    aItem.ItemData.Detail := 'Running on Tablet';
    aItem.Text := BoolToStr(IsTablet, true);

    aItem := TListBoxItem.Create(nil);
    aItem.Parent := ListBox1;
    aItem.ItemData.Detail := 'Phablet [ANDROID/IOS]';
    aItem.Text := BoolToStr(IsLargePhone, true);

    // screen
    aItem := TListBoxGroupHeader.Create(nil);
    aItem.Parent := ListBox1;
    aItem.Margins.Top := 10;
    aItem.Text := 'Screen';

    aItem := TListBoxItem.Create(nil);
    aItem.Parent := ListBox1;
    aItem.ItemData.Detail := 'Phys';
    aItem.Text := diScreenPhis;

    aItem := TListBoxItem.Create(nil);
    aItem.Parent := ListBox1;
    aItem.ItemData.Detail := 'Logic';
    aItem.Text := diScreenLogic;

    aItem := TListBoxItem.Create(nil);
    aItem.Parent := ListBox1;
    aItem.ItemData.Detail := 'Scale';
    aItem.Text := diScale.ToString;

    // network
    aItem := TListBoxGroupHeader.Create(nil);
    aItem.Parent := ListBox1;
    aItem.Margins.Top := 10;
    aItem.Text := 'Network';

    aItem := TListBoxItem.Create(nil);
    aItem.Parent := ListBox1;
    aItem.ItemData.Detail := 'Carrier [ANDROID/IOS]';
    aItem.Text := {$IF defined(ANDROID) or defined(IOS)} diMobileOperator {$ELSE} 'Not support' {$ENDIF};

    aItem := TListBoxItem.Create(nil);
    aItem.Parent := ListBox1;
    aItem.ItemData.Detail := 'Internet [ANDROID/WINDOWS]';
    aItem.Text := 'Connected = ' + BoolToStr(IsNetConnected, true);

    aItem := TListBoxItem.Create(nil);
    aItem.Parent := ListBox1;
    aItem.ItemData.Detail := 'Internet Type [ANDROID/WINDOWS]';
    aItem.Text := TmyConnectionTypeString[IsNetConnectionType];

    aItem := TListBoxItem.Create(nil);
    aItem.Parent := ListBox1;
    aItem.ItemData.Detail := 'Network Type [ANDROID]';
    aItem.Text := TmyNetworkTypeString[IsNetworkType];

    aItem := TListBoxItem.Create(nil);
    aItem.Parent := ListBox1;
    aItem.ItemData.Detail := 'IP address'{$IFDEF ANDROID} + ' (Wi-Fi)'{$ENDIF};
    aItem.Text := diIPAddress;

    aItem := TListBoxItem.Create(nil);
    aItem.Parent := ListBox1;
    aItem.ItemData.Detail := 'MAC address'{$IFDEF ANDROID} + ' (< android 6.0)'{$ENDIF};
    aItem.Text := diMacAddress;

    // net
    aItem := TListBoxGroupHeader.Create(nil);
    aItem.Parent := ListBox1;
    aItem.Margins.Top := 10;
    aItem.Text := 'Sensor';

    aItem := TListBoxItem.Create(nil);
    aItem.Parent := ListBox1;
    aItem.ItemData.Detail := 'GPS Active '{$IFDEF ANDROID} + '[ANDROID]'{$ELSE} +
      'for current platform Result TRUE'{$ENDIF};
    aItem.Text := BoolToStr(IsGPSActive, true);
  end;
end;

end.
