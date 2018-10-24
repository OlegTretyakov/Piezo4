unit CalculatedObjInterface;

interface
  type
  ICalculatedObj = interface (IInterface)
    ['{7E63841A-5D62-4E28-9502-F1F3145F6D75}']
    function GetBoard_SN : LongWord; stdcall;
    function GetBoard_Pos : Byte; stdcall;
    property Board_SN : LongWord read GetBoard_SN;
    property Board_Pos : Byte read GetBoard_Pos;
  end;
implementation

end.