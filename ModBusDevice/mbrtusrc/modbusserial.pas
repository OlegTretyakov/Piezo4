unit ModBusSerial;

interface

uses
  System.Classes,
  ModBusDriver,
  AbstractTag,
  commtypes;

type

  TModBusRTUDriver = class(TModBusDriver)
  protected
    procedure BuildReadPacket(const ATagObj : TAbstractTag; APacket: pIOPacket); override;
    procedure BuildWritePacket(const ATagObj : TAbstractTag; APacket: pIOPacket); override;
    function ParcePacket(APacket : pIOPacket; const ATagObj : TAbstractTag): TProtocolIOResult; override;
  end;

implementation

uses
  System.Math,
  System.SysUtils,
  ProtocolTypes,
  crc16utils;

procedure TModBusRTUDriver.BuildReadPacket(const ATagObj : TAbstractTag; APacket: pIOPacket);
var
  vTagSize : Word;
begin
  vTagSize := ATagObj.Size;
  if vTagSize < 1 then
    exit;
  case ATagObj.MemReadFunction of
    $01,$02,$03,$04:
    begin
      //encode a packet to read input, outputs, register or analog registers
      APacket.ToWriteCount := 8;
      APacket.BufferToWrite[0] := ATagObj.PLCStation and $FF;
      APacket.BufferToWrite[1] := ATagObj.MemReadFunction and $FF;
      APacket.BufferToWrite[2] := (ATagObj.MemAddress and $FF00) shr 8;
      APacket.BufferToWrite[3] := ATagObj.MemAddress and $FF;
      APacket.BufferToWrite[4] := (vTagSize and $FF00) shr 8;
      APacket.BufferToWrite[5] := vTagSize and $FF;
      // computes the CRC
      Calcul_crc(APacket);
    end;

    $07:
    begin
      //encode a packet to read the device status.
      APacket.ToWriteCount := 4;
      APacket.BufferToWrite[0] := ATagObj.PLCStation and $FF;
      APacket.BufferToWrite[1] := $07;
      // Calcula o CRC
      // computes the CRC
      Calcul_crc(APacket);
    end;

    $08:
    begin
      // line test
      APacket.ToWriteCount := 8;
      APacket.BufferToWrite[0] := ATagObj.PLCStation and $FF;
      APacket.BufferToWrite[1] := $08;
      APacket.BufferToWrite[2] := 0;
      APacket.BufferToWrite[3] := 0;
      APacket.BufferToWrite[4] := 0;
      APacket.BufferToWrite[5] := 0;
      Calcul_crc(APacket);
    end;
    else
    begin
      APacket.ToWriteCount := 0;
    end;
  end;

  // computes the size of the incoming packet.
  case ATagObj.MemReadFunction of
    $01..$02:
      APacket.ToReadCount := 5 +(vTagSize div 8)+IfThen((vTagSize mod 8)<>0,1,0);
    $03..$04:
      APacket.ToReadCount := 5+(vTagSize*2);
    $07:
      APacket.ToReadCount := 5;
    $08:
      APacket.ToReadCount := 8;
    else
    begin
      APacket.ToReadCount := 0;
    end;
  end;

end;

procedure TModBusRTUDriver.BuildWritePacket(const ATagObj : TAbstractTag; APacket: pIOPacket);
var
  i, c, c2 : Integer;
  vTagSize : Word;
  vAnalog : IProtocolAnalogTag;
  vDiscrete : IProtocolDiscreteTag;
begin
  inherited;
  vTagSize := 0;
  if Assigned(ATagObj) then
    vTagSize := ATagObj.Size;
  if (vTagSize < 1) then
    Exit;
  try
    case ATagObj.MemWriteFunction of
      $05:
      begin
        if not Supports(ATagObj, IProtocolDiscreteTag, vDiscrete) then
          exit;
        //encodes a packet to write a single coil.
        APacket.ToWriteCount := 8;
        APacket.BufferToWrite[0] := ATagObj.PLCStation and $FF;
        APacket.BufferToWrite[1] := $05;
        APacket.BufferToWrite[2] := (ATagObj.MemAddress and $FF00) shr 8;
        APacket.BufferToWrite[3] := ATagObj.MemAddress and $FF;

        if vDiscrete.ValuesToWrite[0] then
        begin
          APacket.BufferToWrite[4] := $FF;
          APacket.BufferToWrite[5] := $00;
        end else
        begin
          APacket.BufferToWrite[4] := $00;
          APacket.BufferToWrite[5] := $00;
        end;
        // computes the CRC
        Calcul_crc(APacket);
      end;

      $06:
      begin
        // encodes a packet to write a single register.
        if not Supports(ATagObj, IProtocolAnalogTag, vAnalog) then
          exit;
        APacket.ToWriteCount := 8;
        APacket.BufferToWrite[0] := ATagObj.PLCStation and $FF;
        APacket.BufferToWrite[1] := $06;
        APacket.BufferToWrite[2] := (ATagObj.MemAddress and $FF00) shr 8;
        APacket.BufferToWrite[3] := ATagObj.MemAddress and $FF;
        APacket.BufferToWrite[4] := (vAnalog.ValuesToWrite[0] and $FF00) shr 8;
        APacket.BufferToWrite[5] := vAnalog.ValuesToWrite[0] and $FF;
        // computes the CRC
        Calcul_crc(APacket);
      end;

      $0F:
      begin
        //encodes a packet to write multiple coils.
        if not Supports(ATagObj, IProtocolDiscreteTag, vDiscrete) then
          exit;
        APacket.ToWriteCount := (vTagSize div 8)+IfThen((vTagSize mod 8)>0,1,0)+9;
        APacket.BufferToWrite[0] := ATagObj.PLCStation and $FF;
        APacket.BufferToWrite[1] := $0F;
        APacket.BufferToWrite[2] := ((ATagObj.MemAddress) and $FF00) shr 8;
        APacket.BufferToWrite[3] := (ATagObj.MemAddress) and $FF;
        APacket.BufferToWrite[4] := (vTagSize and $FF00) shr 8;
        APacket.BufferToWrite[5] := vTagSize and $FF;
        APacket.BufferToWrite[6] := (vTagSize div 8)+IfThen((vTagSize mod 8)>0,1,0);

        i := 0;
        c2:= 7;
        APacket.BufferToWrite[7] := 0;

        for c := 0 to vTagSize-1 do
        begin
          if vDiscrete.ValuesToWrite[c] then
          begin
            APacket.BufferToWrite[c2] := APacket.BufferToWrite[c2] or (1 shl i);
          end;

          inc(i);
          if i>7 then
          begin
            i:=0;
            inc(c2);
            APacket.BufferToWrite[c2] := 0;
          end;
        end;

        // computes the CRC
        Calcul_crc(APacket);
      end;
      $10:
      begin
        if not Supports(ATagObj, IProtocolAnalogTag, vAnalog) then
          exit;
        // encodes a packet to write multiple registers
        APacket.ToWriteCount := (vTagSize*2)+9;
        APacket.BufferToWrite[0] := ATagObj.PLCStation and $FF;
        APacket.BufferToWrite[1] := $10;
        APacket.BufferToWrite[2] := ((ATagObj.MemAddress) and $FF00) shr 8;
        APacket.BufferToWrite[3] := (ATagObj.MemAddress) and $FF;
        APacket.BufferToWrite[4] := ((vTagSize and $FF00) shr 8);
        APacket.BufferToWrite[5] := vTagSize and $FF;
        APacket.BufferToWrite[6] := (vTagSize*2) and $FF;
        i := 0;
        while (i < vTagSize) do
        begin
          APacket.BufferToWrite[7+i*2] := ((vAnalog.ValuesToWrite[i] and $FF00) shr 8);
          APacket.BufferToWrite[8+i*2] := vAnalog.ValuesToWrite[i] and $FF;
          inc(i);
        end;
        // computes the CRC
        Calcul_crc(APacket);
      end;
      else
      begin
        APacket.ToWriteCount := 0;
      end;
    end;
    // computes the size of the incoming packet.
    if (ATagObj.MemWriteFunction in [$05,$06,$0F,$10]) then
      APacket.ToReadCount := 8
    else
      APacket.ToReadCount := 0;
  finally
    vAnalog := nil;
    vDiscrete := nil;
  end;
end;

function TModBusRTUDriver.ParcePacket(APacket : pIOPacket; const ATagObj : TAbstractTag): TProtocolIOResult;
var
  c,c2 : integer;
  i, vLen : Word;
  vTag : IProtocolPLCTag;
  vAnalog : IProtocolAnalogTag;
  vDiscrete : IProtocolDiscreteTag;
begin
  //if some IO fail.
  Result:=ioOk;

  case APacket.WriteIOResult of
    iorTimeOut:
      Result:=ioTimeOut;
    iorNotReady,
    iorNone:
      Result:=ioDriverError;
    iorPortError:
      Result := ioCommError;
  end;

  if (Result=ioOk)then
  begin
    case APacket.ReadIOResult of
      iorTimeOut:
        Result:=ioTimeOut;
      iorNotReady,
      iorNone:
        Result:=ioDriverError;
      iorPortError:
        Result := ioCommError;
    end;
  end;

  //if the address in the incoming packet is different of the requested
  if (Result=ioOk) and (APacket.BufferToWrite[0]<>APacket.BufferToRead[0]) then
  begin
    Result := ioCommError;
  end;

  //verify the CRC of incoming packet.
  if (Result=ioOk) and ((not Test_crc(APacket.BufferToWrite, APacket.ToWriteCount)) or (not Test_crc(APacket.BufferToRead, APacket.ToReadCount))) then
  begin
    Result := ioCommError;
    exit;
  end;

  if (Result=ioOk) and (ATagObj.Size < 1) then
  begin
    Result := ioTagError;
  end;
  try
    if not Supports(ATagObj, IProtocolPLCTag, vTag) then
    begin
      Result := ioTagError;
      exit;
    end;
    if (Result <> ioOk) then
    begin
      vTag.SetLastError(Result);
      exit;
    end;
    //decodes the packet.

    case APacket.BufferToRead[1] of
      $01,$02: //request to read the digital input/outpus (coils)
      begin
        //where the data decoded will be stored.
        if not Supports(ATagObj, IProtocolDiscreteTag, vDiscrete) then
        begin
          result := ioTagError;
          vTag.SetLastError(Result);
          exit;
        end;

        if Result=ioOk then
        begin
          //vAddress := (APkg.BufferToWrite[2] shl 8) + APkg.BufferToWrite[3];
          vLen     := (APacket.BufferToWrite[4] shl 8) + APacket.BufferToWrite[5];

          if vLen > ATagObj.Size then
          begin
            result := ioPLCError;
            vTag.SetLastError(Result);
            exit;
          end;
          vTag.SetDataChanged(false);
          i := 0;
          c := 0;
          c2:= 3;
          while (i<vLen) and (c2<Length(APacket.BufferToRead)) do
          begin
            if (c=8) then
            begin
              c:=0;
              inc(c2);
            end;
            vDiscrete.ValuesReaded[i]:=IfThen(((Integer(APacket.BufferToRead[c2]) and (1 shl c))=(1 shl c)),1,0)=1;
            inc(i);
            inc(c);
          end;
          vTag.SetLastError(Result);
        end;
      end;


      $03,$04: //request to read analog registers
      begin
        //where the data decoded will be stored.
        if not Supports(ATagObj, IProtocolAnalogTag, vAnalog) then
        begin
          result := ioTagError;
          vTag.SetLastError(Result);
          exit;
        end;

        if Result=ioOk then
        begin
          //vAddress := (APkg.BufferToWrite[2] shl 8) + APkg.BufferToWrite[3];
          vLen     := (APacket.BufferToWrite[4] shl 8) + APacket.BufferToWrite[5];
          if vLen > ATagObj.Size then
          begin
            result := ioPLCError;
            vTag.SetLastError(Result);
            exit;
          end;
          vTag.SetDataChanged(false);
          // data are ok
          for i:=0 to vLen-1 do
          begin
            vAnalog.ValuesReaded[i]:=(word(APacket.BufferToRead[3+(i*2)]) shl 8) + word(APacket.BufferToRead[4+i*2]);
          end;
          vTag.SetLastError(Result);
        end;
      end;
      $05, $06,$0F,$10:// decodes a write
      begin
        vTag.SetDataChanged(false);
        if Result = ioOK then
          vTag.WriteOK;
        vTag.SetLastError(Result);
      end
    else
      begin
        //modbus error handling.
        case APacket.BufferToRead[1] of
          $81:
            Result := ioIllegalFunction;
          $82:
            Result := ioIllegalRegAddress;
          $83:
            Result := ioIllegalValue;
          $84,$85,$86,$87,$88:
            Result := ioPLCError;
        end;
        vTag.SetLastError(Result);
      end;
    end;
  finally
    vTag := nil;
    vAnalog := nil;
    vDiscrete := nil;
  end;
end;

end.
