unit dmReadInterface;

interface
  type
  IdmRead = interface(IInterface)
    ['{B6D6BDCF-C298-467F-8891-A96AC8FCD46F}']
    function Read(Params : Pointer=nil): Boolean; stdcall;
  end;
  
const
C_ReadStart : TGUID = '{BA050394-98BF-48C6-BA27-728139ADCBAD}';  
C_ReadDone  : TGUID = '{80EEA99F-44BE-45B5-9FC6-171D0634D124}'; 
C_ReadError : TGUID = '{099C40A7-32B7-493B-969E-5ABD60D0212C}';

implementation

end.
