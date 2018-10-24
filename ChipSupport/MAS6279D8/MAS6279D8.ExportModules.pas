unit MAS6279D8.ExportModules;

interface

implementation

uses
AbstractStpMethod,
MAS6279D8.MMStartTest,
MAS6279D8.MMPreparePoint,
MAS6279D8.MMInRange,
MAS6279D8.FinalProgrammer;

function StartTestStpClass: TStpMethodClass;stdcall;
begin
  result := Tmasd8StartTest;
end; exports StartTestStpClass;

function PreparePointStpClass: TStpMethodClass;stdcall;
begin
  result := Tmasd8PreparePoint;
end; exports PreparePointStpClass;

function InRangeStpClass: TStpMethodClass;stdcall;
begin
  result := Tmasd8InRange;
end; exports InRangeStpClass;

function FinalProgrammerClass: TFinalProgrammerClass;stdcall;
begin
  result := Tmasd8FinalProgrammer;
end; exports FinalProgrammerClass;

end.
