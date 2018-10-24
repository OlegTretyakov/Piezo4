unit SA4350MC;
interface

uses
  Vcl.Forms,
  Vcl.ComCtrls, Vcl.StdCtrls, Vcl.Buttons, JvExStdCtrls, JvEdit,
  JvValidateEdit, VrLcd, VrLeds, Vcl.Controls, VrControls, VrDigit,
  System.Classes, Vcl.ExtCtrls, FormsControllerInterface, SA4350MCInterface;
  

type
  TSA4350Frm = class(TForm,
                    IShowMdiForm,
                    IFormToFront,
                    IFormProps,
                    IFormOnDestroyEvent,
                    ISA4350MC)
    CurrentTemr: TVrDigitGroup;
    TargetTempr: TVrDigitGroup;
    Speed: TVrDigitGroup;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    TemtrEdit: TJvValidateEdit;
    TimeEdit: TJvValidateEdit;
    SetTemprBtn: TBitBtn;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    InPointLed: TVrUserLed;
    OnExposureLed: TVrUserLed;
    Label7: TLabel;
    Label8: TLabel;
    ChamberOffBtn: TBitBtn;
    TemperatOffBtn: TBitBtn;
    VrClock1: TVrClock;
    StatusBar1: TStatusBar;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    fOnDestroy : TNotifyEvent;
    {IShowMdiForm}
    procedure IShowMdiForm.Show = IShowMdiForm_Show;
    procedure IShowMdiForm_Show; stdcall;
    {IFormToFront}
    procedure IFormToFront.BringToFront = IFormToFront_BringToFront;
    procedure IFormToFront_BringToFront;stdcall;
    {IFormProps}
    function IFormProps.GetHeight = IFormProps_GetHeight;
    function IFormProps.GetTop = IFormProps_GetTop;
    procedure IFormProps.SetTop = IFormProps_SetTop;
    function IFormProps_GetHeight : integer; stdcall;
    function IFormProps_GetTop : integer; stdcall;
    procedure IFormProps_SetTop(const Value : Integer);stdcall;
    {IFormOnDestroyEvent}
    procedure SetOnDestroy(const AEvent : TNotifyEvent);stdcall;
    function GetOnDestroy : TNotifyEvent;stdcall;
    {ISA4350MC}
    procedure BlockForm(ADoBlock : boolean); stdcall;
    procedure SetMessageText(PanelIndex : byte; const AText : string);stdcall;
    procedure SetLigthState(AState: TchLigthState); stdcall;
    procedure UpdateClock(AThen : TDateTime); stdcall;
    procedure SetTargetTempr(const Value : Double);stdcall;
    procedure SetCurrentTempr(const Value : Double);stdcall;
    procedure SetSpeed(const Value : Double);stdcall;
    function GetTargetTempr : Double; stdcall;
    function GetTargetTime : Cardinal; stdcall;
    procedure ISA4350MC.Close = ISA4350MC_Close;
    procedure ISA4350MC_Close;stdcall;
   public
    destructor Destroy; override;
  end;


implementation

uses
System.SysUtils,
System.DateUtils,
Vodopad.Math;

{$R *.dfm}


function CreateForm(AOwner : TComponent;
                     ASetTemprBtnClick,
                     ATemperatOffBtnClick,
                     AChamberOffBtnClick : TNotifyEvent):TComponent; stdcall;
var
vFrm : TSA4350Frm;
begin
  vFrm := TSA4350Frm.Create(AOwner);
  vFrm.SetTemprBtn.OnClick := ASetTemprBtnClick;
  vFrm.TemperatOffBtn.OnClick := ATemperatOffBtnClick;
  vFrm.ChamberOffBtn.OnClick := AChamberOffBtnClick;
  Result := vFrm;
end; exports CreateForm;

destructor TSA4350Frm.Destroy;
begin
  if Assigned(fOnDestroy) then
    fOnDestroy(Self);
  inherited;
end;

procedure TSA4350Frm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TSA4350Frm.SetLigthState(AState : TchLigthState);
begin
  case AState of
    lOff:
    begin
      InPointLed.Active := false;
      OnExposureLed.Active := false;         
      VrClock1.Hours := 0;
      VrClock1.Minutes := 0;
      VrClock1.Seconds := 0;
    end;
    lWatch:
    begin
      InPointLed.Active := true;
      OnExposureLed.Active := false;
    end;
    lExposure:
    begin
      InPointLed.Active := true;
      OnExposureLed.Active := true;
    end;
  end;
end;

procedure TSA4350Frm.SetMessageText(PanelIndex: byte; const AText: string);
begin
  StatusBar1.Panels[PanelIndex].Text := AText;
end;

procedure TSA4350Frm.BlockForm(ADoBlock: boolean);
begin
  SetTemprBtn.Enabled := not ADoBlock;
  ChamberOffBtn.Enabled := not ADoBlock;
  TemperatOffBtn.Enabled := not ADoBlock;
end;

procedure TSA4350Frm.UpdateClock(AThen : TDateTime);
var
vH: Word;
vM, vS : byte;
begin
  SecondToTime(SecondsBetween(Now, AThen), vH, vM, vS);
  VrClock1.Hours := vH;
  VrClock1.Minutes := vM;
  VrClock1.Seconds := vS;
end;

function TSA4350Frm.GetOnDestroy: TNotifyEvent;
begin
  result := fOnDestroy;
end;

function TSA4350Frm.GetTargetTempr: Double;
begin
  Result := TemtrEdit.AsFloat;
end;

function TSA4350Frm.GetTargetTime: Cardinal;
begin
  Result := TimeEdit.AsInteger;
end;

function TSA4350Frm.IFormProps_GetHeight: integer;
begin
  result := Self.Height;
end;

function TSA4350Frm.IFormProps_GetTop: integer;
begin
  Result := Self.Top;
end;

procedure TSA4350Frm.IFormProps_SetTop(const Value: Integer);
begin
  Self.Top := Value;
end;

procedure TSA4350Frm.IFormToFront_BringToFront;
begin
  Self.BringToFront;
end;

procedure TSA4350Frm.IShowMdiForm_Show;
begin
  Self.Show;
end;

procedure TSA4350Frm.ISA4350MC_Close;
begin
  Self.Close;
end;

procedure TSA4350Frm.SetCurrentTempr(const Value: Double);
begin
  CurrentTemr.Value := Value;
end;

procedure TSA4350Frm.SetOnDestroy(const AEvent: TNotifyEvent);
begin
  fOnDestroy := AEvent;
end;

procedure TSA4350Frm.SetSpeed(const Value: Double);
begin
  Speed.Value := Value;
end;

procedure TSA4350Frm.SetTargetTempr(const Value: Double);
begin
  TargetTempr.Value := Value;
end;

end.
