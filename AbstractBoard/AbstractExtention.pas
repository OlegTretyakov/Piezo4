unit AbstractExtention;

interface

uses
  System.Classes;


type
  TAbstractExtention = class(TComponent)
   protected
    procedure AfterCreate; virtual;
    procedure BeforeDestroy; virtual;
   public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TAbstractExtentionClass = class of TAbstractExtention;
  TExtentionsClasses = array of TAbstractExtentionClass;

implementation


{ TAbstractExtention }

procedure TAbstractExtention.AfterCreate;
begin
end;

procedure TAbstractExtention.BeforeDestroy;
begin
end;

constructor TAbstractExtention.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AfterCreate;
end;

destructor TAbstractExtention.Destroy;
begin
  BeforeDestroy;
  inherited Destroy;
end;

end.
