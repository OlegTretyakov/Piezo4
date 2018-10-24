unit VdpDMConfAccess;

interface
  type
   TDBConnParams = record
   DBName,
   UserName,
   Password,
   LibraryName :ShortString;
  end;
  pDBConnParams = ^TDBConnParams;

  IConfigAccess = interface (IInterface)
  ['{281416E2-4FB4-4F40-A51B-1ACE95613163}']     
     function LoadConnParams(AParams: pDBConnParams): boolean; stdcall;
     function Connect(AParams: pDBConnParams): boolean; stdcall;
  end;
implementation

end.
