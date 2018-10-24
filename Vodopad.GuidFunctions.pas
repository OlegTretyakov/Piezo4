unit Vodopad.GuidFunctions;

interface
uses Data.DB, FIBQuery, System.SysUtils;
  
function IsEqualGuidAsField(AField : TField; AGuid : TGUID):boolean;
function FieldToGUID(AField : TField):TGUID; overload;
function FieldToGUID(AField : TFIBXSQLVAR):TGUID;overload;
function GuidIsNull(AGUID : TGUID) : boolean;

implementation

function FieldToGUID(AField : TField):TGUID;
var s : String;
begin
  s := AField.AsString;
  result := fibGUID_NULL;
  if (s <> '') then
  try
    result := StringToGUID(s);
  except
  end;
end;

function FieldToGUID(AField : TFIBXSQLVAR):TGUID;
var s : String;
begin
  s := AField.AsString;
  result := fibGUID_NULL;
  if (s <> '') then
  try
    result := StringToGUID(s);
  except
  end;
end;

function IsEqualGuidAsField(AField : TField; AGuid : TGUID):boolean;
begin
  result := IsEqualGUID(FieldToGUID(AField), AGuid);
end;

function GuidIsNull(AGUID : TGUID) : boolean;
begin
  result := IsEqualGUID(AGUID, fibGUID_NULL);
end;
end.
