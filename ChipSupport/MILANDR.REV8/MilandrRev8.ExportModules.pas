unit MilandrRev8.ExportModules;

interface

implementation

uses
AbstractStpMethod,
MilandrRev8.MMStartTest,
MilandrRev8.MMPreparePoint,
MilandrRev8.MMInRange,
MilandrRev8.FinalProgrammer;

function StartTestStpClass: TStpMethodClass;stdcall;
begin
  result := TMilRev8StartTest;
end; exports StartTestStpClass;

function PreparePointStpClass: TStpMethodClass;stdcall;
begin
  result := TMilRev8PreparePoint;
end; exports PreparePointStpClass;

function InRangeStpClass: TStpMethodClass;stdcall;
begin
  result := TMilRev8InRange;
end; exports InRangeStpClass;

function FinalProgrammerClass: TFinalProgrammerClass;stdcall;
begin
  result := TMilRev8FinalProgrammer;
end; exports FinalProgrammerClass;

end.
