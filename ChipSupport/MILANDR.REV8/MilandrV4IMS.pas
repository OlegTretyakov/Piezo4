unit MilandrV4IMS;

interface
uses Windows, classes, IniFiles, SysUtils, MCPInterface, PCListAccess;
 type

  TMilandrV4Reg = record
   public
    FIFTH   :smallint;
    FOUR    :smallint;
    CUB     :smallint;
    VC      :byte;
    SC      :byte;
    AMPL    :byte;
    FCH     :byte;
    DIVisor :byte;
    CDACF   :smallint;
    CDACC   :smallint;
    OFS     :byte;
    SCALE   :smallint;
    LIN     :smallint;
    INF     :smallint;
    TEST    :byte;
    CMD     :byte;  
    procedure ToBuffer(const Buffer; var PacketMarker: Word);
    procedure FromBuffer(const Buffer; var PacketMarker: Word);
    procedure SaveToStream(AStream : TMemoryStream);
    procedure LoadFromStream(AStream : TStream);
    procedure Write(AWriter: TWriter);
    procedure Read(AReader: TReader);
   end;  

  TMilandrV4Registers = class(TRegisters)
  protected
    function GetVvarBitValue(ATag : byte): smallInt;  override;
    procedure SetVvarBitValue(ATag : byte; AValue: smallInt); override;
    function GetBitValue(AIndex : byte):smallint; override;
    procedure SetBitValue(AIndex : byte; AValue: smallInt); override;
  public
    Registers : TMilandrV4Reg;
    class function BoardSupported(ADevClass, AHwVersion, AFWVersion : Word):boolean;override;
    function CreateClone : TRegisters; override;
    procedure Assign(ASource : TRegisters; AssignOptions : TAssignOpt = aoAll); overload;override;
    procedure Read(AReader : TReader); override;
    procedure Write(AWriter : TWriter); override;
    procedure SaveToStream(AStream : TMemoryStream); override;
    procedure LoadFromStream(AStream : TStream); override; 
    procedure SaveToIni(AIniFile : TCustomIniFile; const ASection : string); override;
    procedure LoadFromIni(AIniFile : TCustomIniFile; const ASection : string); override;
    function IsIdenty(ASource: TRegisters; AOptions : TAssignOpt = aoAll): boolean; overload;override;
    class function GetClassType : TRegistersClass; override;
    class function ChipName: ShortString; override;
    class function GUID : TGUID; override; 
    class procedure DefaultProcessingList(APCListAccess : IPCListAccess; AMode : byte); override;
    class function BitMinIndex : byte; override;
    class function BitMaxIndex : byte; override;
    class function BitName(AIndex : byte):ShortString; override;
    class function InGridBitVisible(AIndex : byte):boolean; override;
    class function BitMinValue(AIndex : byte):smallint; override; 
    class function BitMeanValue(AIndex : byte):smallint; override;
    class function BitMaxValue(AIndex : byte):smallint; override;
    class function BitDefValue(AIndex : byte):smallint; override;
    class function VvarBitMinIndex : byte; override;
    class function VvarBitMaxIndex : byte; override;
    class function VvarRegName(ATag : byte) :ShortString; override;
    class function VvarBitMinValue(ATag : byte): smallInt; override;
    class function VvarBitMaxValue(ATag : byte): smallInt; override;
    property VvarBitValue[ATag : byte]:smallint read GetVvarBitValue write SetVvarBitValue;
    class function NededProgPower : boolean; override;
    class function SupportedCommandByte(ACommand : byte) : boolean; override;
    class function GetExecutedPosChipCommand(ACommand : byte) : byte; override;
    procedure ParceBoardRead(const Buffer; Count: Word; var PacketMarker: Word); override;
    class function GetBitsCommandByte(ACommand : byte) : byte; override;
    procedure BuildTxPacket(const Buffer; var PacketMarker: Word); override;
    class function IsTempWorkRegister(APinsState : Word): boolean; override;
    class function GetTempPinsState : Word; override;
    class function GetRomPinsState : Word; override;
    class function MaxCDACBit : byte; override;
    procedure SetCDACMean; override;
    procedure SetCDACMin; override;
    procedure SetCDACMax; override;
    procedure IncCDAC(ABitNum : byte); override;
    procedure DecCDAC(ABitNum : byte); override;
    procedure CheckCDAC(MeasFreq, NominalFreq : double; var ACurrentBit : byte); override;
    class function EndCDAC(ACurrentBit : byte) : boolean; override;
    function CDACIsLimited : boolean; override;
    procedure ModeVVarForce; override;
    procedure ModeVVar; override;
    procedure ModeTcOff; override;
  end;



function GetIMSClassType : TRegistersClass; stdcall;

implementation
uses Math, vdMath;

const
C_INF_Min = 0;
C_INF_Mean = 31;
C_INF_Max = 63;

C_LIN_Min = 0;
C_LIN_Mean = 127;
C_LIN_Max = 255;

C_SCALE_Min = 0;
C_SCALE_Mean = 15;
C_SCALE_Max = 31;

C_CUB_Min = 0;
C_CUB_Mean = 7;
C_CUB_Max = 15; 

C_FOUR_Min = 0;
C_FOUR_Mean = 7;
C_FOUR_Max = 15; 

C_FIFTH_Min = 0;
C_FIFTH_Mean = 7;
C_FIFTH_Max = 15;

C_CDACC_Min = 0;
C_CDACC_Mean = 7;
C_CDACC_Max = 15;

C_CDACF_Min = 0;
C_CDACF_Mean = 3;
C_CDACF_Max = 7;

C_OFS_Min = 0;
C_OFS_Mean = 4;
C_OFS_Max = 7;

C_VvarMinIndex = 1;
C_VvarMaxIndex = 6;

C_CDACMaxBit = 7;

C_BufferSize = 7;

C_bit_write_temp = $B1;
C_bit_write_eeprom = $B2;
C_bit_read_temp = $A1;
C_bit_read_eeprom = $A2;

function GetIMSClassType : TRegistersClass; stdcall;
begin
  result := TMilandrV4Registers.GetClassType;
end; exports GetIMSClassType;

 
{$REGION 'Milandr functions'}

procedure TMilandrV4Reg.FromBuffer(const Buffer; var PacketMarker: Word);
var vPacket : pByteArray;
begin
  vPacket := pByteArray(@Buffer);
  FillChar(self, sizeOf(TMilandrV4Reg), 0);
  if ((vPacket[PacketMarker] and $80) > 0) then CMD := CMD or $02;
  if ((vPacket[PacketMarker] and $40) > 0) then CMD := CMD or $01;
  if ((vPacket[PacketMarker] and $20) > 0) then TEST := TEST or $08;
  if ((vPacket[PacketMarker] and $10) > 0) then TEST := TEST or $04;

  if ((vPacket[PacketMarker] and $08) > 0) then TEST := TEST or $02;
  if ((vPacket[PacketMarker] and $04) > 0) then TEST := TEST or $01;

  if ((vPacket[PacketMarker] and $02) > 0) then INF := INF or $20;
  if ((vPacket[PacketMarker] and $01) > 0) then INF := INF or $10;
  inc(PacketMarker);
  if ((vPacket[PacketMarker] and $80) > 0) then INF := INF or $08;
  if ((vPacket[PacketMarker] and $40) > 0) then INF := INF or $04;
  if ((vPacket[PacketMarker] and $20) > 0) then INF := INF or $02;
  if ((vPacket[PacketMarker] and $10) > 0) then INF := INF or $01;

  if ((vPacket[PacketMarker] and $08) > 0) then LIN := LIN or $80;
  if ((vPacket[PacketMarker] and $04) > 0) then LIN := LIN or $40;
  if ((vPacket[PacketMarker] and $02) > 0) then LIN := LIN or $20;
  if ((vPacket[PacketMarker] and $01) > 0) then LIN := LIN or $10;
  inc(PacketMarker);
  if ((vPacket[PacketMarker] and $80) > 0) then LIN := LIN or $08;
  if ((vPacket[PacketMarker] and $40) > 0) then LIN := LIN or $04;
  if ((vPacket[PacketMarker] and $20) > 0) then LIN := LIN or $02;
  if ((vPacket[PacketMarker] and $10) > 0) then LIN := LIN or $01;

  if ((vPacket[PacketMarker] and $08) > 0) then SCALE := SCALE or $10;
  if ((vPacket[PacketMarker] and $04) > 0) then SCALE := SCALE or $08;
  if ((vPacket[PacketMarker] and $02) > 0) then SCALE := SCALE or $04;
  if ((vPacket[PacketMarker] and $01) > 0) then SCALE := SCALE or $02;
  inc(PacketMarker);
  if ((vPacket[PacketMarker] and $80) > 0) then SCALE := SCALE or $01;

  if ((vPacket[PacketMarker] and $40) > 0) then OFS := OFS or $04;
  if ((vPacket[PacketMarker] and $20) > 0) then OFS := OFS or $02;
  if ((vPacket[PacketMarker] and $10) > 0) then OFS := OFS or $01;
  
  if ((vPacket[PacketMarker] and $08) > 0) then CDACC := CDACC or $08;
  if ((vPacket[PacketMarker] and $04) > 0) then CDACC := CDACC or $04;
  if ((vPacket[PacketMarker] and $02) > 0) then CDACC := CDACC or $02;
  if ((vPacket[PacketMarker] and $01) > 0) then CDACC := CDACC or $01;
  inc(PacketMarker);
  {if ((vPacket[PacketMarker] and $80) > 0) then CDACF := CDACF or $20;
  if ((vPacket[PacketMarker] and $40) > 0) then CDACF := CDACF or $10;
  if ((vPacket[PacketMarker] and $20) > 0) then CDACF := CDACF or $08; }
  if ((vPacket[PacketMarker] and $10) > 0) then CDACF := CDACF or $04;
  if ((vPacket[PacketMarker] and $08) > 0) then CDACF := CDACF or $02;
  if ((vPacket[PacketMarker] and $04) > 0) then CDACF := CDACF or $01;

  if ((vPacket[PacketMarker] and $02) > 0) then DIVisor := DIVisor or $02;
  if ((vPacket[PacketMarker] and $01) > 0) then  DIVisor := DIVisor or $01;
  inc(PacketMarker);
  if ((vPacket[PacketMarker] and $80) > 0) then FCH := FCH or $01;
  
  if ((vPacket[PacketMarker] and $40) > 0) then AMPL := AMPL or $01;
  
  if ((vPacket[PacketMarker] and $20) > 0) then SC := SC or $01;
  
  if ((vPacket[PacketMarker] and $10) > 0) then VC := VC or $01;
  
  if ((vPacket[PacketMarker] and $08) > 0) then CUB := CUB or $08;
  if ((vPacket[PacketMarker] and $04) > 0) then CUB := CUB or $04;
  if ((vPacket[PacketMarker] and $02) > 0) then CUB := CUB or $02;
  if ((vPacket[PacketMarker] and $01) > 0) then CUB := CUB or $01;
  inc(PacketMarker);
  if ((vPacket[PacketMarker] and $80) > 0) then FOUR := FOUR or $08;
  if ((vPacket[PacketMarker] and $40) > 0) then FOUR := FOUR or $04;
  if ((vPacket[PacketMarker] and $20) > 0) then FOUR := FOUR or $02;
  if ((vPacket[PacketMarker] and $10) > 0) then FOUR := FOUR or $01;
  
  if ((vPacket[PacketMarker] and $08) > 0) then FIFTH := FIFTH or $08;
  
  if ((vPacket[PacketMarker] and $04) > 0) then FIFTH := FIFTH or $04;
  if ((vPacket[PacketMarker] and $02) > 0) then FIFTH := FIFTH or $02;
  if ((vPacket[PacketMarker] and $01) > 0) then FIFTH := FIFTH or $01;
  inc(PacketMarker);
end;


procedure TMilandrV4Reg.ToBuffer(const Buffer; var PacketMarker: Word);
var vPacket : pByteArray;
begin
  vPacket := pByteArray(@Buffer);
  FillChar(vPacket[PacketMarker], C_BufferSize, 0);
  if ((CMD and $02) > 0) then vPacket[PacketMarker] := vPacket[PacketMarker] or $80;
  if ((CMD and $01) > 0) then vPacket[PacketMarker] := vPacket[PacketMarker] or $40;

  if ((TEST and $08) > 0) then vPacket[PacketMarker] := vPacket[PacketMarker] or $20;
  if ((TEST and $04) > 0) then vPacket[PacketMarker] := vPacket[PacketMarker] or $10;
  if ((TEST and $02) > 0) then vPacket[PacketMarker] := vPacket[PacketMarker] or $08;
  if ((TEST and $01) > 0) then vPacket[PacketMarker] := vPacket[PacketMarker] or $04;
  
  if ((INF and $20) > 0) then vPacket[PacketMarker] := vPacket[PacketMarker] or $02;
  if ((INF and $10) > 0) then vPacket[PacketMarker] := vPacket[PacketMarker] or $01;
  inc(PacketMarker);
  if ((INF and $08) > 0) then vPacket[PacketMarker] := vPacket[PacketMarker] or $80;
  if ((INF and $04) > 0) then vPacket[PacketMarker] := vPacket[PacketMarker] or $40;
  if ((INF and $02) > 0) then vPacket[PacketMarker] := vPacket[PacketMarker] or $20;
  if ((INF and $01) > 0) then vPacket[PacketMarker] := vPacket[PacketMarker] or $10;

  if ((LIN and $80) > 0) then vPacket[PacketMarker] := vPacket[PacketMarker] or $08;
  if ((LIN and $40) > 0) then vPacket[PacketMarker] := vPacket[PacketMarker] or $04;
  if ((LIN and $20) > 0) then vPacket[PacketMarker] := vPacket[PacketMarker] or $02;
  if ((LIN and $10) > 0) then vPacket[PacketMarker] := vPacket[PacketMarker] or $01;
  inc(PacketMarker);
  if ((LIN and $08) > 0) then vPacket[PacketMarker] := vPacket[PacketMarker] or $80;
  if ((LIN and $04) > 0) then vPacket[PacketMarker] := vPacket[PacketMarker] or $40;
  if ((LIN and $02) > 0) then vPacket[PacketMarker] := vPacket[PacketMarker] or $20;
  if ((LIN and $01) > 0) then vPacket[PacketMarker] := vPacket[PacketMarker] or $10;

  if ((SCALE and $10) > 0) then vPacket[PacketMarker] := vPacket[PacketMarker] or $08;
  if ((SCALE and $08)  > 0) then vPacket[PacketMarker] := vPacket[PacketMarker] or $04;
  if ((SCALE and $04)  > 0) then vPacket[PacketMarker] := vPacket[PacketMarker] or $02;
  if ((SCALE and $02)  > 0) then vPacket[PacketMarker] := vPacket[PacketMarker] or $01;
  inc(PacketMarker);
  if ((SCALE and $01) > 0) then vPacket[PacketMarker] := vPacket[PacketMarker] or $80;

  if ((OFS and $04) > 0) then vPacket[PacketMarker] := vPacket[PacketMarker] or $40;
  if ((OFS and $02) > 0) then vPacket[PacketMarker] := vPacket[PacketMarker] or $20;
  if ((OFS and $01) > 0) then vPacket[PacketMarker] := vPacket[PacketMarker] or $10;

  if ((CDACC and $08) > 0) then vPacket[PacketMarker] := vPacket[PacketMarker] or $08;
  if ((CDACC  and $04) > 0) then vPacket[PacketMarker] := vPacket[PacketMarker] or $04;
  if ((CDACC  and $02) > 0) then vPacket[PacketMarker] := vPacket[PacketMarker] or $02;
  if ((CDACC  and $01) > 0) then vPacket[PacketMarker] := vPacket[PacketMarker] or $01;
  inc(PacketMarker);
  if ((CDACF and $20) > 0) then vPacket[PacketMarker] := vPacket[PacketMarker] or $80;  
  if ((CDACF and $10) > 0) then vPacket[PacketMarker] := vPacket[PacketMarker] or $40;
  if ((CDACF and $08) > 0) then vPacket[PacketMarker] := vPacket[PacketMarker] or $20;
  if ((CDACF and $04) > 0) then vPacket[PacketMarker] := vPacket[PacketMarker] or $10;
  if ((CDACF and $02) > 0) then vPacket[PacketMarker] := vPacket[PacketMarker] or $08;
  if ((CDACF and $01) > 0) then vPacket[PacketMarker] := vPacket[PacketMarker] or $04;

  if ((DIVisor and $02) > 0) then vPacket[PacketMarker] := vPacket[PacketMarker] or $02;
  if ((DIVisor and $01) > 0) then vPacket[PacketMarker] := vPacket[PacketMarker] or $01;
  inc(PacketMarker);
  if (FCH = 1) then vPacket[PacketMarker] := vPacket[PacketMarker] or $80;

  if (AMPL = 1) then vPacket[PacketMarker] := vPacket[PacketMarker] or $40;

  if (SC = 1) then vPacket[PacketMarker] := vPacket[PacketMarker] or $20;

  if (VC = 1) then vPacket[PacketMarker] := vPacket[PacketMarker] or $10;

  if ((CUB and $08) > 0) then vPacket[PacketMarker] := vPacket[PacketMarker] or $08;
  if ((CUB and $04) > 0) then vPacket[PacketMarker] := vPacket[PacketMarker] or $04;
  if ((CUB and $02) > 0) then vPacket[PacketMarker] := vPacket[PacketMarker] or $02;
  if ((CUB and $01) > 0) then vPacket[PacketMarker] := vPacket[PacketMarker] or $01;
  inc(PacketMarker);
  if ((FOUR and $08) > 0) then vPacket[PacketMarker] := vPacket[PacketMarker] or $80;
  if ((FOUR and $04) > 0) then vPacket[PacketMarker] := vPacket[PacketMarker] or $40;
  if ((FOUR and $02) > 0) then vPacket[PacketMarker] := vPacket[PacketMarker] or $20;
  if ((FOUR and $01) > 0) then vPacket[PacketMarker] := vPacket[PacketMarker] or $10;

  if ((FIFTH and $08) > 0) then vPacket[PacketMarker] := vPacket[PacketMarker] or $08;
  if ((FIFTH and $04) > 0) then vPacket[PacketMarker] := vPacket[PacketMarker] or $04;
  if ((FIFTH and $02) > 0) then vPacket[PacketMarker] := vPacket[PacketMarker] or $02;
  if ((FIFTH and $01) > 0) then vPacket[PacketMarker] := vPacket[PacketMarker] or $01;
  inc(PacketMarker);
end;


procedure TMilandrV4Reg.LoadFromStream(AStream: TStream);
begin
  AStream.Read(self, SizeOf(TMilandrV4Reg));
end;

procedure TMilandrV4Reg.SaveToStream(AStream: TMemoryStream);
begin
   AStream.Write(self, SizeOf(TMilandrV4Reg));
end;

procedure TMilandrV4Reg.Write(AWriter: TWriter);
begin
  AWriter.Write(self, SizeOf(TMilandrV4Reg));
end;

procedure TMilandrV4Reg.Read(AReader: TReader);
begin
  AReader.Read(self, SizeOf(TMilandrV4Reg));
end;

{$ENDREGION}



{ TMilandrV5Registers }


class function TMilandrV4Registers.MaxCDACBit: byte;
begin
  result := C_CDACMaxBit;
end;

procedure TMilandrV4Registers.SetCDACMax;
begin
  Registers.CDACC := C_CDACC_Max;
  Registers.CDACF := C_CDACF_Max;
end;

procedure TMilandrV4Registers.SetCDACMean;
begin
  Registers.CDACC := C_CDACC_Mean+1;
  Registers.CDACF := C_CDACF_Min;
end;

procedure TMilandrV4Registers.SetCDACMin;
begin
  Registers.CDACC := C_CDACC_Min;
  Registers.CDACF := C_CDACF_Min;
end;

procedure TMilandrV4Registers.CheckCDAC(MeasFreq, NominalFreq: double;
  var ACurrentBit: byte);
begin
  if (MeasFreq < NominalFreq) then
    DecCDAC(ACurrentBit);
  Inc(ACurrentBit);
  if not EndCDAC(ACurrentBit)  then
    IncCDAC(ACurrentBit);
end; 

procedure TMilandrV4Registers.IncCDAC(ABitNum: byte);
begin
  if (ABitNum < FixedTrunc(Log2(C_CDACC_Max + 1))) then
    Registers.CDACC := Registers.CDACC or ((C_CDACC_Mean+1) shr ABitNum)
  else
    Registers.CDACF := Registers.CDACF
      or ((C_CDACF_Mean+1) shr (ABitNum - FixedTrunc(Log2(C_CDACC_Max + 1))));
end;

procedure TMilandrV4Registers.DecCDAC(ABitNum: byte);
begin
 if (ABitNum < FixedTrunc(Log2(C_CDACC_Max + 1))) then
    Registers.CDACC := Registers.CDACC and not ((C_CDACC_Mean+1) shr ABitNum)
  else
    Registers.CDACF := Registers.CDACF
      and not ((C_CDACF_Mean+1) shr (ABitNum - FixedTrunc(Log2(C_CDACC_Max + 1))))
end;

class function TMilandrV4Registers.EndCDAC(ACurrentBit: byte): boolean;
begin
  result := (ACurrentBit > MaxCDACBit);
end;  

function TMilandrV4Registers.CDACIsLimited: boolean;
begin
  result := (((Registers.CDACC = C_CDACC_Max) and (Registers.CDACF = C_CDACF_Max))
    or ((Registers.CDACC = C_CDACC_Min) and (Registers.CDACF = C_CDACF_Min)));
end;
class function TMilandrV4Registers.ChipName: ShortString;
begin
  result := 'Milandr V4';
end;

class function TMilandrV4Registers.IsTempWorkRegister(APinsState : Word): boolean;
begin
  result := not GetBitState(APinsState, 7) and not GetBitState(APinsState, 6)
  and GetBitState(APinsState, 5) and not GetBitState(APinsState, 3) and not GetBitState(APinsState, 2);

  {(not (pDAInput in APinsState)) and (not (pCLKInput in APinsState)) and
  (pMoutInput in APinsState) and
  (not (pDAState in APinsState)) and (not (pCLKState in APinsState));}
end;  

class function TMilandrV4Registers.GetRomPinsState: Word;
begin
  result := $E0;//[pDAInput, pCLKInput, pMoutInput];
end;

class function TMilandrV4Registers.GetTempPinsState: Word;
begin
  result := $10;//[pMoutInput];
end;

class function TMilandrV4Registers.InGridBitVisible(AIndex: byte): boolean;
begin
  result := true;
end;

class function TMilandrV4Registers.GetBitsCommandByte(
  ACommand: Byte): byte;
begin
  result := 0;
  case ACommand of
    pcWriteTemp: result := C_bit_write_temp;
    pcWriteEprom: result := C_bit_write_eeprom;
    pcReadTemp: result := C_bit_read_temp;
    pcReadEprom: result := C_bit_read_eeprom;
  end;
end;

class function TMilandrV4Registers.GetExecutedPosChipCommand(
  ACommand: byte): byte;
begin
  case ACommand of
    C_bit_write_temp: result := pcWriteTemp;
    C_bit_write_eeprom: result := pcWriteEprom;
    C_bit_read_temp: result := pcReadTemp;
    C_bit_read_eeprom: result := pcReadEprom;
  end;
end;

class function TMilandrV4Registers.GUID: TGUID;
const cGUID : TGUID = '{8F8C9FF6-D5E7-44CD-979E-1DD370840AC4}';
begin
  result := cGUID;
end;

class procedure TMilandrV4Registers.DefaultProcessingList(
  APCListAccess: IPCListAccess; AMode: byte);
begin
  APCListAccess.ClearList;
  case AMode of
    0:
    begin  ///wr-test
      APCListAccess.AddItem(pcWriteTemp);
      APCListAccess.AddItem(pcReadTemp);
    end;
    1: //1:cdac-test,
    begin
      APCListAccess.AddItem(pcWriteTemp);
    end;
    2: //2:task-list
    begin
      APCListAccess.AddItem(pcWriteTemp);
      APCListAccess.AddItem(pcReadTemp);
    end;
  end;
end;

function TMilandrV4Registers.CreateClone: TRegisters;
begin
  result := TMilandrV4Registers.Create;
  result.Assign(self);
end;

procedure TMilandrV4Registers.Assign(ASource: TRegisters; AssignOptions : TAssignOpt = aoAll);
var i : byte;
begin
  if ASource is TMilandrV4Registers then
  case AssignOptions of
    aoAll:
      for I := BitMinIndex to BitMaxIndex do
        self.BitValue[i] := TMilandrV4Registers(ASource).BitValue[i];
    aoCDAC:
    begin
      Registers.CDACC := TMilandrV4Registers(ASource).Registers.CDACC;
      Registers.CDACF := TMilandrV4Registers(ASource).Registers.CDACF;
    end;
    aoVvar:
      for I := VvarBitMinIndex to VvarBitMaxIndex do
        self.VvarBitValue[i] := TMilandrV4Registers(ASource).VvarBitValue[i];
  end;
end;  

function TMilandrV4Registers.IsIdenty(ASource: TRegisters;
                                  AOptions : TAssignOpt = aoAll): boolean;
var i : byte;
begin
  result := (ASource is TMilandrV4Registers);
  if not result then
    exit;
  case AOptions of
    aoAll:
    for i := BitMinIndex+2 to BitMaxIndex do
    begin
      result := self.BitValue[i] = TMilandrV4Registers(ASource).BitValue[i];
      if not result then
        break;
    end;
    aoCDAC:
    begin
      result :=(Registers.CDACC = TMilandrV4Registers(ASource).Registers.CDACC)
           and (Registers.CDACF = TMilandrV4Registers(ASource).Registers.CDACF);
    end;
    aoVvar:
    for i := VvarBitMinIndex to VvarBitMaxIndex do
    begin
      result := self.VvarBitValue[i] = TMilandrV4Registers(ASource).VvarBitValue[i];
      if not result then
        break;
    end;
  end;
end;


class function TMilandrV4Registers.BitDefValue(AIndex: byte): smallint;
const cMilDefs : array [0..15] of smallint =
  (0, 0, C_INF_Mean, C_LIN_Mean,
  C_SCALE_Mean, C_CUB_Mean, C_FOUR_Mean, C_FIFTH_Mean, C_CDACC_Mean, C_CDACF_Mean,
  0, 0, 1, 0, 0, 0);
begin
  result := cMilDefs[AIndex];
end;

class function TMilandrV4Registers.BitMaxIndex: byte;
begin
  result := 15;
end;

class function TMilandrV4Registers.BitMaxValue(AIndex: byte): smallint;
const cValsMax : array [0..15] of smallint=(15, 4, C_INF_Max, C_LIN_Max,
  C_SCALE_Max, C_CUB_Max, C_FOUR_Max, C_FIFTH_Max, C_CDACC_Max, C_CDACF_Max,
  3, 1, 1, 7, 1, 1);
begin
  result := cValsMax[AIndex];
end;

class function TMilandrV4Registers.BitMeanValue(AIndex: byte): smallint;
const cValsMeans : array [0..15] of smallint =
  (7, 2, C_INF_Mean, C_LIN_Mean,
  C_SCALE_Mean, C_CUB_Mean, C_FOUR_Mean, C_FIFTH_Mean, C_CDACC_Mean, C_CDACF_Mean,
  0, 0, 1, 3, 0, 0);
begin
  result := cValsMeans[AIndex];
end;

class function TMilandrV4Registers.BitMinIndex: byte;
begin
  result := 0;
end;

class function TMilandrV4Registers.BitMinValue(AIndex: byte): smallint;
const cValsMin : array [0..15] of smallint=(0, 0, C_INF_Min, C_LIN_Min,
  C_SCALE_Min, C_CUB_Min, C_FOUR_Min, C_FIFTH_Min, C_CDACC_Min, C_CDACF_Min,
  0, 0, 0, 0, 0, 0);
begin
  result := cValsMin[AIndex];
end;

class function TMilandrV4Registers.BitName(AIndex: byte): ShortString;
const cBitNames : array[0..15] of ShortString =
      ('TEST','CMD','INF','LIN','SCALE','CUB','FOUR','FIFTH',
      'CDACC','CDACF','DIV','SC','VC','OFS','AMPL','FCH');
begin
  result := cBitNames[AIndex];
end;

class function TMilandrV4Registers.BoardSupported(ADevClass, AHwVersion,
  AFWVersion: Word): boolean;
begin
  Result :=  ADevClass in [2,3];
  if not Result then
    Exit;
  case ADevClass of
    2:
    begin
      Result := (HiByte(AHwVersion) = 2)
      and (byte(AHwVersion) >= 0)
      and (HiByte(AFWVersion) = 2)
      and (byte(AFWVersion) >= 1);
    end;
    3:
    begin
      Result := (HiByte(AHwVersion) = 1)
      and (byte(AHwVersion) >= 0)
      and (HiByte(AFWVersion) = 1)
      and (byte(AFWVersion) >= 0);
    end;
  end;
end;

procedure TMilandrV4Registers.LoadFromIni(AIniFile: TCustomIniFile;
  const ASection: string);
var i : byte;
begin
  for I := BitMinIndex to BitMaxIndex do
    BitValue[i] := SmallInt(AIniFile.ReadInteger(ASection, BitName(i), BitValue[i]));
end;

procedure TMilandrV4Registers.LoadFromStream(AStream: TStream);
begin
  Registers.LoadFromStream(AStream);
end;

class function TMilandrV4Registers.NededProgPower: boolean;
begin
  result := true;
end;  

procedure TMilandrV4Registers.BuildTxPacket(const Buffer;
                              var PacketMarker: Word);
var vPacket : pByteArray;
begin
  vPacket := pByteArray(@Buffer);
  if (vPacket[0] in [C_bit_write_temp, C_bit_write_eeprom]) then
     Registers.ToBuffer(Buffer, PacketMarker);
end;

procedure TMilandrV4Registers.ParceBoardRead(const Buffer; Count: Word;
  var PacketMarker: Word);
begin
  if (Count - PacketMarker >= C_BufferSize) then
    Registers.FromBuffer(Buffer, PacketMarker);
end;

procedure TMilandrV4Registers.Read(AReader: TReader);
begin
  Registers.Read(AReader);
end;

class function TMilandrV4Registers.SupportedCommandByte(ACommand: byte): boolean;
begin
  result := (ACommand in [C_bit_read_temp,
                          C_bit_read_eeprom,
                          C_bit_write_temp,
                          C_bit_write_eeprom]);
end;

procedure TMilandrV4Registers.Write(AWriter: TWriter);
begin
  Registers.Write(AWriter);
end;

procedure TMilandrV4Registers.SaveToIni(AIniFile: TCustomIniFile;
  const ASection: string);
var i : byte;
begin
  for I := BitMinIndex to BitMaxIndex do
    AIniFile.WriteInteger(ASection, BitName(i), BitValue[i]);
end;

procedure TMilandrV4Registers.SaveToStream(AStream: TMemoryStream);
begin
  Registers.SaveToStream(AStream);
end;

procedure TMilandrV4Registers.SetBitValue(AIndex: byte; AValue: smallInt);
begin
  case AIndex of
    0: Registers.TEST := EnsureRange(AValue, BitMinValue(AIndex), BitMaxValue(AIndex));
    1: Registers.CMD := EnsureRange(AValue, BitMinValue(AIndex), BitMaxValue(AIndex));
    2: Registers.INF := EnsureRange(AValue, BitMinValue(AIndex), BitMaxValue(AIndex));
    3: Registers.LIN := EnsureRange(AValue, BitMinValue(AIndex), BitMaxValue(AIndex));
    4: Registers.SCALE := EnsureRange(AValue, BitMinValue(AIndex), BitMaxValue(AIndex));
    5: Registers.CUB := EnsureRange(AValue, BitMinValue(AIndex), BitMaxValue(AIndex));
    6: Registers.FOUR := EnsureRange(AValue, BitMinValue(AIndex), BitMaxValue(AIndex));
    7: Registers.FIFTH := EnsureRange(AValue, BitMinValue(AIndex), BitMaxValue(AIndex));
    8: Registers.CDACC := EnsureRange(AValue, BitMinValue(AIndex), BitMaxValue(AIndex));
    9: Registers.CDACF := EnsureRange(AValue, BitMinValue(AIndex), BitMaxValue(AIndex));
    10: Registers.DIVisor := EnsureRange(AValue, BitMinValue(AIndex), BitMaxValue(AIndex));
    11: Registers.SC := EnsureRange(AValue, BitMinValue(AIndex), BitMaxValue(AIndex));
    12: Registers.VC := EnsureRange(AValue, BitMinValue(AIndex), BitMaxValue(AIndex));
    13: Registers.OFS := EnsureRange(AValue, BitMinValue(AIndex), BitMaxValue(AIndex));
    14: Registers.AMPL := EnsureRange(AValue, BitMinValue(AIndex), BitMaxValue(AIndex));
    15: Registers.FCH := EnsureRange(AValue, BitMinValue(AIndex), BitMaxValue(AIndex));
  end;
end;  

function TMilandrV4Registers.GetBitValue(AIndex: byte): smallint;
begin
  case AIndex of
    0: result := Registers.TEST;
    1: result := Registers.CMD;
    2: result := Registers.INF;
    3: result := Registers.LIN;
    4: result := Registers.SCALE;
    5: result := Registers.CUB;
    6: result := Registers.FOUR;
    7: result := Registers.FIFTH;
    8: result := Registers.CDACC;
    9: result := Registers.CDACF;
    10: result := Registers.DIVisor;
    11: result := Registers.SC;
    12: result := Registers.VC;
    13: result := Registers.OFS;
    14: result := Registers.AMPL;
    15: result := Registers.FCH;
  end;
end;

class function TMilandrV4Registers.GetClassType: TRegistersClass;
begin
  result := TMilandrV4Registers;
end; 

class function TMilandrV4Registers.VvarBitMaxIndex: byte;
begin
  result := C_VvarMaxIndex;
end;

class function TMilandrV4Registers.VvarBitMaxValue(ATag: byte): smallInt;
const C_VvarMaxs : array [C_VvarMinIndex..C_VvarMaxIndex] of smallInt=
(C_INF_Max, C_LIN_Max-1, {C_SCALE_Max,} C_CUB_Max, C_FOUR_Max, C_FIFTH_Max, C_OFS_Max);
begin
  result := C_VvarMaxs[ATag];
end;

class function TMilandrV4Registers.VvarBitMinIndex: byte;
begin
  result := C_VvarMinIndex;
end;

class function TMilandrV4Registers.VvarBitMinValue(ATag: byte): smallInt;
const C_VvarMins : array [C_VvarMinIndex..C_VvarMaxIndex] of smallInt=
(C_INF_Min, C_LIN_Min, {C_SCALE_Min,} C_CUB_Min, C_FOUR_Min, C_FIFTH_Min, C_OFS_Min);
begin
  result := C_VvarMins[ATag];
end;

class function TMilandrV4Registers.VvarRegName(ATag: byte): ShortString;
const C_VvarMilNames : array [C_VvarMinIndex..C_VvarMaxIndex] of ShortString = ('INF','LIN',{'SCALE',}'CUB','FOUR','FIFTH','OFS');
begin
  result := C_VvarMilNames[ATag];
end;

procedure TMilandrV4Registers.ModeTcOff;
begin
  Registers.LIN := 255;
end;

procedure TMilandrV4Registers.ModeVVar;
begin
  Registers.TEST := 6;
end;

procedure TMilandrV4Registers.ModeVVarForce;
begin
  Registers.TEST := 6;
  {Registers.INF := 0;
  Registers.LIN := 255;
  Registers.SCALE := 0;
  Registers.CUB := 0;
  Registers.FOUR := 0;
  Registers.FIFTH := 0; }
end;  

function TMilandrV4Registers.GetVvarBitValue(ATag: byte): smallInt;
begin
  result := 0;
  case ATag of 
    1: result := Registers.INF;
    2: result := Registers.LIN;
    //3: result := Registers.SCALE;
    3: result := Registers.CUB;
    4: result := Registers.FOUR;
    5: result := Registers.FIFTH;
    6: result := Registers.OFS;
  end;
end;

procedure TMilandrV4Registers.SetVvarBitValue(ATag: byte; AValue: smallInt);
begin
  case ATag of 
   1: Registers.INF := EnsureRange(AValue, VvarBitMinValue(ATag), VvarBitMaxValue(ATag));
   2: Registers.LIN := EnsureRange(AValue, VvarBitMinValue(ATag), VvarBitMaxValue(ATag));
  // 3: Registers.SCALE := EnsureRange(AValue, VvarBitMinValue(ATag), VvarBitMaxValue(ATag));
   3: Registers.CUB := EnsureRange(AValue, VvarBitMinValue(ATag), VvarBitMaxValue(ATag));
   4: Registers.FOUR := EnsureRange(AValue, VvarBitMinValue(ATag), VvarBitMaxValue(ATag));
   5: Registers.FIFTH := EnsureRange(AValue, VvarBitMinValue(ATag), VvarBitMaxValue(ATag));
   6: Registers.OFS := EnsureRange(AValue, VvarBitMinValue(ATag), VvarBitMaxValue(ATag));
  end;
end;

end.
