unit CodeBindingsList;

interface
uses
  Data.Bind.Components, Classes, CodeBindings;

type
  TCodeBindingsList = class(TBindingsList)
  private
  protected
  public
    function Bind(const Source : TComponent) : IComponentSource; virtual;
    function BindList(const Source : TComponent) : IListComponentSource; virtual;
  end;

implementation

{ TCodeBindingsList }

function TCodeBindingsList.Bind(const Source: TComponent): IComponentSource;
var
  LBindingState : TBindingState;
begin
  LBindingState := TBindingState.Create(self);
  LBindingState.Source := Source;
  LBindingState.Direction := SourceToTarget;

  Result := TComponentSource.Create(LBindingState) as IComponentSource;
end;

function TCodeBindingsList.BindList(
  const Source: TComponent): IListComponentSource;
var
  LBindingState : TBindingState;
begin
  LBindingState := TBindingState.Create(self);
  LBindingState.Source := Source;
  LBindingState.SourceIsList := True;
  LBindingState.Direction := Bidirectional;

  Result := TListComponentSource.Create(LBindingState) as IListComponentSource;
end;

end.
