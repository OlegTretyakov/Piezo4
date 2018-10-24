unit mbPluginManagerInterface;

interface
 uses DeviceModule; 
 type
 IMBPM = interface(IInterface)
   ['{9D27D436-2A2F-4B15-AAA1-00AAC3EFA487}'] 
   function FindModuleClass(ID, Version : Word; var oModuleClass : TDeviceModuleClass):Boolean;stdcall;
 end;
implementation

end.
