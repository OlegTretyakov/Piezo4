unit InfCoefsInterface;

interface
  uses VvarMatrixIF;
  type
  IInfCoefs = interface(IVvarCharactTable)
    ['{51A5E249-8B0A-4940-82C9-3FC051186F34}']
    function GetTemprRegInfCoeff : double; stdcall;
    function GetRegTemprInfCoeff : double; stdcall;
    property TemprRegInfCoeff: double read GetTemprRegInfCoeff;
    property RegTemprInfCoeff: double read GetRegTemprInfCoeff;
  end;
implementation

end.
