unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Memo, FMX.ScrollBox, FMX.Controls.Presentation, FMX.StdCtrls;

type
  TForm2 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

uses
  FMX.ZDeviceInfo;

procedure TForm2.Button1Click(Sender: TObject);
var
  di: TZDeviceInfo;
begin
  Memo1.Lines.Clear;
  di := TZDeviceInfo.Create;
  try
    Memo1.Lines.Add('Platform: ' + di.Platform);
    Memo1.Lines.Add('PlatformVer: ' + di.PlatformVer);
    Memo1.Lines.Add('Architecture: ' + di.Architecture);
    Memo1.Lines.Add('Architecture2: ' + di.Architecture2);
    Memo1.Lines.Add('IsIntel: ' + di.IsIntel.ToString(TUseBoolStrs.True));
    Memo1.Lines.Add('Device: ' + di.Device);
    Memo1.Lines.Add('---------');
    Memo1.Lines.Add('DeviceID: ' + di.DeviceID);
    Memo1.Lines.Add('---------');
    Memo1.Lines.Add('MacAddress: ' + di.MacAddress);
    Memo1.Lines.Add('IPAddress: ' + di.IPAddress);
    Memo1.Lines.Add('LangID: ' + di.LangID);
    Memo1.Lines.Add('ScreenPhis: ' + di.ScreenPhis);
    Memo1.Lines.Add('ScreenLogic: ' + di.ScreenLogic);
    Memo1.Lines.Add('Scale: ' + di.Scale.ToString);
    Memo1.Lines.Add('MobileOperator: ' + di.MobileOperator);
    Memo1.Lines.Add('TimeZone: ' + di.TimeZone.ToString);

    Memo1.Lines.Add('IsNetConnected: ' + di.IsNetConnected.ToString(TUseBoolStrs.True));
    Memo1.Lines.Add('NetworkConnectionType: ' + TZNetworkConnectionTypeString[di.NetworkConnectionType]);
    Memo1.Lines.Add('MobileDataType: ' + TZMobileDataTypeString[di.MobileDataType]);
  finally
    di.Free;
  end;
end;

end.
