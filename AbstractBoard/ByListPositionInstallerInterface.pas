unit ByListPositionInstallerInterface;

interface
  uses
  AbstractExtention;

 type
  IByListPositionInstaller = Interface(IInterface)
  ['{3F1AEA42-F535-4160-997A-F5DBFBFE3DAC}']
    function InstalledPositionExtClass : TAbstractExtentionClass;stdcall;
  end;
  
implementation


end.
