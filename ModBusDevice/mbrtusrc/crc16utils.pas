unit crc16utils;



interface

uses
  commtypes;

{:
Verifies an package of BYTES ultil their length - 2 and returns @true
if the value calculated is the same on that is stored in the last 2 bytes
of the package.

If the Pkg has 10 bytes of length, it will calculate the CRC using the
first 8 bytes and it will check the result with bytes 9 and 10 of Pkg.
The byte number 9 is the most significative.

@param(Pkg BYTES. Buffer that stores the data that will be checked the CRC-16.
       Must have at least 2 bytes of length.)

@returns(@true if the CRC-16 calculated is the same that is stored on the last 2
         bytes of Pkg.)
}
function Test_crc(const ABuffer : TIOBuffer; ABufferLen : Word):Boolean;


{:
Calculate the CRC-16 of the package until their size - 2. If the package has
10 bytes of length, it will use to calculate the first 8 bytes and will store
the result of the calculation on bytes 9 and 10. The byte number 9 is the most
significative.

@param(Pkg BYTES. Buffer that contains the data that will be used to calculate
       the CRC-16 and that will store the result on the last 2 bytes. Must have
       at least 2 bytes of length.)

@returns(A Cardinal number with the CRC-16 calculated with length of Pkg - 2.)
}
function Calcul_crc(APkg: pIOPacket): Word;

implementation

function Test_crc(const ABuffer : TIOBuffer; ABufferLen : Word):Boolean;
var
  crc,j,carry_flag,a,
  i,n:Word;
begin
  n := ABufferLen-2;
  crc := $FFFF;
  i := 0;
  while (i < n) do
  begin
    crc := crc xor Word(ABuffer[i]);
    for j:=0 to 7 do
    begin
      a := crc;
      carry_flag := a and $0001;
      crc := crc shr 1;
      if (carry_flag = 1) then
        crc := crc xor $A001;
    end;
    inc(i);
  end;
  Result := ((n+2)<=ABufferLen) and ((Word(ABuffer[n+1])=(crc shr 8)) or (Word(ABuffer[n])=(crc and 255)));
end;

function Calcul_crc(APkg: pIOPacket):Word;
var
  crc,j,carry_flag,a,
  i,n:Word;
begin
  n:=APkg.ToWriteCount-2;
  crc := $FFFF;
  i := 0;
  while (i < n) do
  begin
    crc :=crc xor Word(APkg.BufferToWrite[i]);
    for j:=0 to 7 do
    begin
      a := crc;
      carry_flag := a and $0001;
      crc := crc shr 1;
      if (carry_flag = 1) then
        crc := crc xor $A001;
    end;
    inc(i);
  end;
  APkg.BufferToWrite[n+1] := ((crc and $FF00) shr 8);
  APkg.BufferToWrite[n]   := (crc and 255);
  result := crc;
end;


end.
 
