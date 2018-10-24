unit AppLoggerModule;

interface
{$IFDEF VD7LOG}
uses LoggerInterface, LoggerInit;


var
AppLogger : ILogger=nil;
{$ENDIF}
implementation
{$IFDEF VD7LOG}
uses SysUtils;

procedure InitAppLogger;
var
vConfig : pLogClassConfig;
begin
  New(vConfig);
  try
     vConfig.ConfigGUID := StringToGUID('{3D671997-3D07-4BEA-AB58-E026D71C7FCD}');
     vConfig.EnabledLevels := [lInfo, lEvent, lDebug, lWarning, lError,
                              {$IFDEF DEBUG}lEnter, lLeave, {$ENDIF}
                              lException, lExceptionOS];
     vConfig.DefaultLevelsView := [lInfo, lEvent, lWarning];
     vConfig.FileExtension := 'aplog';
     vConfig.ArhiveExtension := 'aplog7z';
     vConfig.ModuleName := 'Приложение';
     vLoggerFunct.AddLogger(vConfig, AppLogger);
  finally
    Dispose(vConfig);
  end;
end;

initialization
  InitAppLogger;

finalization
  AppLogger := nil;
{$ENDIF}
end.
