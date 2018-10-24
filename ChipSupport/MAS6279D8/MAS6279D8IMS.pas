unit MAS6279D8IMS;

interface
uses WinApi.Windows, System.classes, System.IniFiles, System.SysUtils, ChipAbstract;
  type
  TMas6279D8reg = record
    TEST    :byte;
    INF     :smallint;
    LIN     :smallint;
    SQ      :smallint;
    CUB     :smallint;
    FOUR    :smallint;
    FIFTH   :smallint;
    OFS     :byte;
    TCXO    :byte;
    CDACC   :smallint;
    DRV     :byte;
    CDACF   :smallint;
    DIVisor :byte;
    SC      :byte;
    XOPD    :byte;
    NF    :byte;
    DA,
    CLK : byte;
    {procedure ToBuffer(const Buffer; var PacketMarker: Word);
    procedure FromBuffer(const Buffer; Count: Word; var PacketMarker: Word);  }
    procedure SaveToStream(AStream : TMemoryStream);
    procedure LoadFromStream(AStream : TStream);
   end;

  TMas6279D8Registers = class(TChipAbstract)
  protected
    function GetBitValue(AIndex : byte):smallint; override;
    procedure SetBitValue(AIndex : byte; AValue: smallInt); override;
  public
    Registers : TMas6279D8reg;
    function CreateClone(AOwner : TComponent) : TChipAbstract; override;
    procedure SaveToStream(AStream : TMemoryStream); override;
    procedure LoadFromStream(AStream : TStream); override;
    class function ChipClassType : TChipClass; override;
    class function ChipName: String; override;
    class function ChipGUID : TGUID; override;
    class function BitMinIndex : byte; override;
    class function BitMaxIndex : byte; override;
    class function BitName(AIndex : byte):String; override;
    class function BitMinValue(AIndex : byte):smallint; override; 
    class function BitMeanValue(AIndex : byte):smallint; override;
    class function BitMaxValue(AIndex : byte):smallint; override;
    class function BitDefValue(AIndex : byte):smallint; override;
  end;




implementation  
uses MAS6279D8.Consts, System.Math;


function GetChipClass : TChipClass; stdcall;
begin
  result := TMas6279D8Registers;
end; exports GetChipClass;

{$REGION 'Mas functions'}

procedure TMas6279D8reg.LoadFromStream(AStream: TStream);
begin
  AStream.Read(self, SizeOf(TMas6279D8reg));
end;

procedure TMas6279D8reg.SaveToStream(AStream: TMemoryStream);
begin
   AStream.Write(self, SizeOf(TMas6279D8reg));
end;

{$ENDREGION}


{ TMas6279D8Registers }  



class function TMas6279D8Registers.ChipName: String;
begin
  result := C_ChipName;
end;

class function TMas6279D8Registers.ChipGUID: TGUID;
begin
  result := C_GUID;
end;

function TMas6279D8Registers.CreateClone(AOwner : TComponent): TChipAbstract;
begin
  result := TMas6279D8Registers.Create(AOwner);
  result.Assign(self);
end;

class function TMas6279D8Registers.BitDefValue(AIndex: byte): smallint;
begin
  result := C_ValsDef[AIndex];
end;

class function TMas6279D8Registers.BitMaxIndex: byte;
begin
  result := C_BitMaxIndex;
end;

class function TMas6279D8Registers.BitMaxValue(AIndex: byte): smallint;
begin
  result := C_ValsMax[AIndex];
end;

class function TMas6279D8Registers.BitMeanValue(AIndex: byte): smallint;
begin
  result := C_ValsMeans[AIndex];
end;

class function TMas6279D8Registers.BitMinIndex: byte;
begin
  result := C_BitMinIndex;
end;

class function TMas6279D8Registers.BitMinValue(AIndex: byte): smallint;
begin
  result := C_ValsMin[AIndex];
end;

class function TMas6279D8Registers.BitName(AIndex: byte): String;
begin
  result := C_RegNames[AIndex];
end;

procedure TMas6279D8Registers.LoadFromStream(AStream: TStream);
begin
  Registers.LoadFromStream(AStream);
end;

procedure TMas6279D8Registers.SaveToStream(AStream: TMemoryStream);
begin
  Registers.SaveToStream(AStream);
end;

function TMas6279D8Registers.GetBitValue(AIndex: byte): smallint;
begin
  case AIndex of
    0: result := Registers.TEST;
    1: Result := Registers.DA;
    2: Result := Registers.CLK;
    3: result := Registers.INF;
    4: result := Registers.LIN;
    5: result := Registers.SQ;
    6: result := Registers.CUB;
    7: result := Registers.FOUR;
    8: result := Registers.FIFTH;
    9: result := Registers.OFS;
    10: result := Registers.TCXO;
    11: result := Registers.CDACC;
    12: result := Registers.DRV;
    13: result := Registers.CDACF;
    14: result := Registers.DIVisor;
    15: result := Registers.SC;
    16: result := Registers.XOPD;
    17: result := Registers.NF;
  end;
end;

procedure TMas6279D8Registers.SetBitValue(AIndex: byte; AValue: smallInt);
begin
  case AIndex of
    0: Registers.TEST := EnsureRange(AValue, BitMinValue(AIndex), BitMaxValue(AIndex));
    1: Registers.DA := EnsureRange(AValue, BitMinValue(AIndex), BitMaxValue(AIndex));
    2: Registers.CLK := EnsureRange(AValue, BitMinValue(AIndex), BitMaxValue(AIndex));
    3: Registers.INF := EnsureRange(AValue, BitMinValue(AIndex), BitMaxValue(AIndex));
    4: Registers.LIN := EnsureRange(AValue, BitMinValue(AIndex), BitMaxValue(AIndex));
    5: Registers.SQ := EnsureRange(AValue, BitMinValue(AIndex), BitMaxValue(AIndex));
    6: Registers.CUB := EnsureRange(AValue, BitMinValue(AIndex), BitMaxValue(AIndex));
    7: Registers.FOUR := EnsureRange(AValue, BitMinValue(AIndex), BitMaxValue(AIndex));
    8: Registers.FIFTH := EnsureRange(AValue, BitMinValue(AIndex), BitMaxValue(AIndex));
    9: Registers.OFS := EnsureRange(AValue, BitMinValue(AIndex), BitMaxValue(AIndex));
    10: Registers.TCXO := EnsureRange(AValue, BitMinValue(AIndex), BitMaxValue(AIndex));
    11: Registers.CDACC := EnsureRange(AValue, BitMinValue(AIndex), BitMaxValue(AIndex));
    12: Registers.DRV := EnsureRange(AValue, BitMinValue(AIndex), BitMaxValue(AIndex));
    13: Registers.CDACF := EnsureRange(AValue, BitMinValue(AIndex), BitMaxValue(AIndex));
    14: Registers.DIVisor := EnsureRange(AValue, BitMinValue(AIndex), BitMaxValue(AIndex));
    15: Registers.SC := EnsureRange(AValue, BitMinValue(AIndex), BitMaxValue(AIndex));
    16: Registers.XOPD := EnsureRange(AValue, BitMinValue(AIndex), BitMaxValue(AIndex));
    17: Registers.NF := EnsureRange(AValue, BitMinValue(AIndex), BitMaxValue(AIndex));
  end;
end;

class function TMas6279D8Registers.ChipClassType: TChipClass;
begin
  result := TMas6279D8Registers;
end;

end.
