unit CodeBindingsList;

interface
uses
  Data.Bind.Components, Classes, CodeBindings;

type
  TCodeBindingsList = class(TBindingsList)
  private
  protected
  public
    function Bind(const Source : TComponent) : ISourceBinding; virtual;
  end;

implementation

{ TCodeBindingsList }

function TCodeBindingsList.Bind(const Source: TComponent): ISourceBinding;
var
  LControlBindingState : TControlBindingState;
begin
  LControlBindingState := TControlBindingState.Create(self);
  LCOntrolBindingState.Source := Source;

  Result := TSourceBinding.Create(LControlBindingState) as ISourceBinding;
end;

end.
