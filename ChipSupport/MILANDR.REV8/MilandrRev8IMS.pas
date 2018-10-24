unit MilandrRev8IMS;

interface
uses WinApi.Windows, System.classes, System.IniFiles, System.SysUtils, ChipAbstract;
 type

  TMilandrRev8Reg = record
   public
    FIFTH   :smallint;
    FOUR    :smallint;
    CUB     :smallint;
    VTUNE      :byte;
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
    procedure SaveToStream(AStream : TMemoryStream);
    procedure LoadFromStream(AStream : TStream);
    procedure Write(AWriter: TWriter);
    procedure Read(AReader: TReader);
   end;  

  TMilandrRev8Registers = class(TChipAbstract)
  protected
    function GetBitValue(AIndex : byte):smallint; override;
    procedure SetBitValue(AIndex : byte; AValue: smallInt); override;
  public
    Registers : TMilandrRev8Reg;
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
uses MilandrRev8.Consts, System.Math;


function GetChipClass : TChipClass; stdcall;
begin
  result := TMilandrRev8Registers;
end; exports GetChipClass;


{$REGION 'Milandr functions'}


procedure TMilandrRev8Reg.LoadFromStream(AStream: TStream);
begin
  AStream.Read(self, SizeOf(TMilandrRev8Reg));
end;

procedure TMilandrRev8Reg.SaveToStream(AStream: TMemoryStream);
begin
   AStream.Write(self, SizeOf(TMilandrRev8Reg));
end;

procedure TMilandrRev8Reg.Write(AWriter: TWriter);
begin
  AWriter.Write(self, SizeOf(TMilandrRev8Reg));
end;

procedure TMilandrRev8Reg.Read(AReader: TReader);
begin
  AReader.Read(self, SizeOf(TMilandrRev8Reg));
end;

{$ENDREGION}



{ TMilandrV5Registers }


class function TMilandrRev8Registers.ChipName: String;
begin
  result := C_ChipName;
end;

class function TMilandrRev8Registers.ChipGUID: TGUID;
begin
  result := C_GUID;
end;

class function TMilandrRev8Registers.BitDefValue(AIndex: byte): smallint;
begin
  result := C_ValsDefs[AIndex];
end;

class function TMilandrRev8Registers.BitMaxIndex: byte;
begin
  result := C_BitMaxIndex;
end;

class function TMilandrRev8Registers.BitMaxValue(AIndex: byte): smallint;
begin
  result := C_ValsMax[AIndex];
end;

class function TMilandrRev8Registers.BitMeanValue(AIndex: byte): smallint;
begin
  result := C_ValsMeans[AIndex];
end;

class function TMilandrRev8Registers.BitMinIndex: byte;
begin
  result := 0;
end;

class function TMilandrRev8Registers.BitMinValue(AIndex: byte): smallint;
begin
  result := C_ValsMin[AIndex];
end;

class function TMilandrRev8Registers.BitName(AIndex: byte): String;
begin
  result := C_RegNames[AIndex];
end;

procedure TMilandrRev8Registers.LoadFromStream(AStream: TStream);
begin
  Registers.LoadFromStream(AStream);
end;

procedure TMilandrRev8Registers.SaveToStream(AStream: TMemoryStream);
begin
  Registers.SaveToStream(AStream);
end;


procedure TMilandrRev8Registers.SetBitValue(AIndex: byte; AValue: smallInt);
begin
  case AIndex of
    C_TEST_Index: Registers.TEST := EnsureRange(AValue, BitMinValue(AIndex), BitMaxValue(AIndex));
    C_INF_Index: Registers.INF := EnsureRange(AValue, BitMinValue(AIndex), BitMaxValue(AIndex));
    C_LIN_Index: Registers.LIN := EnsureRange(AValue, BitMinValue(AIndex), BitMaxValue(AIndex));
    C_SCALE_Index: Registers.SCALE := EnsureRange(AValue, BitMinValue(AIndex), BitMaxValue(AIndex));
    C_CUB_Index: Registers.CUB := EnsureRange(AValue, BitMinValue(AIndex), BitMaxValue(AIndex));
    C_FOUR_Index: Registers.FOUR := EnsureRange(AValue, BitMinValue(AIndex), BitMaxValue(AIndex));
    C_FIFTH_Index: Registers.FIFTH := EnsureRange(AValue, BitMinValue(AIndex), BitMaxValue(AIndex));
    C_OFS_Index: Registers.OFS := EnsureRange(AValue, BitMinValue(AIndex), BitMaxValue(AIndex));
    C_CDACC_Index: Registers.CDACC := EnsureRange(AValue, BitMinValue(AIndex), BitMaxValue(AIndex));
    C_CDACF_Index: Registers.CDACF := EnsureRange(AValue, BitMinValue(AIndex), BitMaxValue(AIndex));
    C_DIV_Index: Registers.DIVisor := EnsureRange(AValue, BitMinValue(AIndex), BitMaxValue(AIndex));
    C_SC_Index: Registers.SC := EnsureRange(AValue, BitMinValue(AIndex), BitMaxValue(AIndex));
    C_VTUNE_Index: Registers.VTUNE := EnsureRange(AValue, BitMinValue(AIndex), BitMaxValue(AIndex));
    C_AMPL_Index: Registers.AMPL := EnsureRange(AValue, BitMinValue(AIndex), BitMaxValue(AIndex));
    C_FCH_Index: Registers.FCH := EnsureRange(AValue, BitMinValue(AIndex), BitMaxValue(AIndex));
  end;
end;  

function TMilandrRev8Registers.GetBitValue(AIndex: byte): smallint;
begin
  case AIndex of
    C_TEST_Index: result := Registers.TEST;
    C_INF_Index: result := Registers.INF;
    C_LIN_Index: result := Registers.LIN;
    C_SCALE_Index: result := Registers.SCALE;
    C_CUB_Index: result := Registers.CUB;
    C_FOUR_Index: result := Registers.FOUR;
    C_FIFTH_Index: result := Registers.FIFTH;
    C_OFS_Index: result := Registers.OFS;
    C_CDACC_Index: result := Registers.CDACC;
    C_CDACF_Index: result := Registers.CDACF;
    C_DIV_Index: result := Registers.DIVisor;
    C_SC_Index: result := Registers.SC;
    C_VTUNE_Index: result := Registers.VTUNE;
    C_AMPL_Index: result := Registers.AMPL;
    C_FCH_Index: result := Registers.FCH;
  end;
end;

class function TMilandrRev8Registers.ChipClassType: TChipClass;
begin
  result := TMilandrRev8Registers;
end; 

end.
