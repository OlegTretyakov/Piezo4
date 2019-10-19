unit ExtentionsListInterface;

interface
  uses
    AbstractExtention;

  type

  IExtentions = interface(IInterface)
    ['{28813195-9089-49F0-BA79-C79946593BC9}']
    function GetItem(AIndex : Word):TAbstractExtention; stdcall;
    function Install(const AExtentionClass : TAbstractExtentionClass):Boolean; overload; stdcall;
    function Install(const AExtentionClass : TAbstractExtentionClass; var AExtention):Boolean; overload; stdcall;
    function Find(const AExtentionClass : TAbstractExtentionClass;
                            var AExtention):boolean; overload; stdcall;
    function Find(var AExtIdx : Word; IID : TGUID; out AExtention):boolean; overload; stdcall;
    function Find(var AExtIdx : Word; IID : TGUID):boolean; overload; stdcall;
    function Find(IID : TGUID; out Obj):boolean; overload; stdcall;
    function Find(var AExtIdx : Word; const AExtentionClass : TAbstractExtentionClass;
                            var AExtention):boolean; overload; stdcall;
    function Find(var AExtIdx : Word;
                    const AExtentionClass : TAbstractExtentionClass):boolean; overload; stdcall;
    function Count : Word; stdcall;
    property Item[AIndex : Word]:TAbstractExtention read GetItem; default;
    procedure Delete(const AExtentionClass : TAbstractExtentionClass); stdcall;
    procedure Clear; stdcall;
  end;

  IExtentionsEventSink = interface(IInterface)
    ['{67A664D4-2982-45DE-B32D-F7DDF0DA9C47}']
    procedure OnBeforeDelete(Sender: TObject; AIndex : integer); stdcall;
    procedure OnAfterDelete(Sender: TObject; AIndex : integer); stdcall;
    procedure OnAfterAdd(Sender: TObject; AIndex : integer); stdcall;
  end;


implementation

end.
