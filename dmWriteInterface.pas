unit dmWriteInterface;

interface
  type
  IdmWrite = interface(IInterface)
    ['{55A70648-7FD3-465F-8B7A-A97F530769D6}']
    function Write(Params : Pointer=nil) : boolean; stdcall;
  end; 

const
C_WriteStart : TGUID = '{7138DBEE-C191-41AE-9946-567833006E08}';
C_WriteDone  : TGUID = '{92E489DD-52E3-48EA-8ABE-7B1491C6CC5F}';
C_WriteError : TGUID = '{3E23A7BE-788D-463E-A758-88C4F31B684E}';
implementation

end.
