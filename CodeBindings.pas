unit CodeBindings;

interface
uses
  Data.Bind.Components, Classes;

type

  TBindingState = class
  private
    FSource : TObject;
    FDestination: TObject;
    FPropertyName: String;
    FActive : Boolean;
    FBindingsList : TBindingsList;
    FTrack: Boolean;
  public
    constructor Create(BindingsList : TBindingsList); virtual;
    property BindingsList : TBindingsList read FBindingsList;
    property Source : TObject read FSource write FSource;
    property Destination : TObject read FDestination write FDestination;
    property PropertyName : String read FPropertyName write FPropertyName;
    property Active : Boolean read FActive write FActive;
    property Track : Boolean read FTrack write FTrack;
  end;

  IComponentTarget = interface
  ['{225A2C76-E6C0-40EC-9396-69150EBE96C8}']
    function Active : IComponentTarget;
    function Inactive : IComponentTarget;
    function Track : IComponentTarget;
  end;

  IBaseSource = interface
  ['{D18910B1-188F-434A-BB74-B171F024F00A}']
    function GetBindingState: TBindingState;
    property BindingState : TBindingState read GetBindingState;
  end;

  IComponentSource = interface(IBaseSource)
  ['{D16A2933-9497-4E8F-AB39-20B3D350D6D6}']
    function ToComponent(Name : TComponent; PropertyName : string): IComponentTarget;
  end;

  TBaseTarget = class(TInterfacedObject)
  protected
    FBindingState : TBindingState;
  public
    constructor Create(BindingState : TBindingState); reintroduce;
    destructor Destroy; override;
  end;

  TComponentTarget = class(TBaseTarget, IComponentTarget)
  public
    destructor Destroy; override;
    function Active : IComponentTarget;
    function Inactive : IComponentTarget;
    function Track : IComponentTarget;
  end;

  TBaseSource = class(TInterfacedObject)
  private
    FBindingState : TBindingState;
  protected
    function GetBindingState: TBindingState;
  public
    property BindingState : TBindingState read GetBindingState;
    constructor Create(BindingState : TBindingState); reintroduce;
  end;

  TComponentSource = class(TBaseSource, IComponentSource)
  public
    function ToComponent(Name : TComponent; PropertyName : string): IComponentTarget;
  end;


implementation


constructor TBindingState.Create(BindingsList: TBindingsList);
begin
  FBindingsList := BindingsList;
  FActive := True;
  FTrack := False;
end;

{ TSourceBinding }


function TComponentSource.ToComponent(
  Name: TComponent; PropertyName : string): IComponentTarget;
begin
  FBindingState.Destination := Name;
  FBindingState.PropertyName := PropertyName;

  Result := TComponentTarget.Create(FBindingState) as IComponentTarget;
end;


{ TDestinationBinding }

function TComponentTarget.Active : IComponentTarget;
begin
  FBindingState.Active := True;
  Result := self;
end;


destructor TComponentTarget.Destroy;
var
  LLink : TLinkControlToProperty;
begin
  LLink := TLinkControlToProperty.Create(nil);
  LLink.BindingsList := FBindingState.BindingsList;
  LLink.Control := TComponent(FBindingState.Source);
  LLink.Component := TComponent(FBindingState.Destination);
  LLink.ComponentProperty := FBindingState.PropertyName;
  LLink.Track := FBindingState.Track;
  LLink.Active := FBindingState.Active;

  inherited;
end;

function TComponentTarget.Inactive : IComponentTarget;
begin
  FBindingState.Active := False;
  Result := self;
end;

function TComponentTarget.Track: IComponentTarget;
begin
  FBindingState.Track := True;
  Result := self;
end;

{ TBaseSource }

constructor TBaseSource.Create(BindingState: TBindingState);
begin
  inherited Create;
  FBindingState := BindingState;
end;


function TBaseSource.GetBindingState: TBindingState;
begin
  Result := FBindingState;
end;

{ TBaseTarget }

constructor TBaseTarget.Create(BindingState: TBindingState);
begin
  FBindingState := BindingState;
end;

destructor TBaseTarget.Destroy;
begin
  FBindingState.Free;
  inherited;
end;

end.
