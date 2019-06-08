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
  end;

implementation

{ TCodeBindingsList }

function TCodeBindingsList.Bind(const Source: TComponent): IComponentSource;
var
  LBindingState : TBindingState;
begin
  LBindingState := TBindingState.Create(self);
  LBindingState.Source := Source;

  Result := TComponentSource.Create(LBindingState) as IComponentSource;
end;

end.
