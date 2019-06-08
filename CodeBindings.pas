unit CodeBindings;

interface
uses
  Data.Bind.Components, Classes;

type

  TCustomBindingState = class
  private
    FTrack: Boolean;
  protected
    FSource : TObject;
    FDestination: TObject;
    FPropertyName: String;
    FActive : Boolean;
    FBindingsList : TBindingsList;
  public
    constructor Create(BindingsList : TBindingsList); virtual;
    property Source : TObject read FSource write FSource;
    property Destination : TObject read FDestination write FDestination;
    property PropertyName : String read FPropertyName write FPropertyName;
    property Active : Boolean read FActive write FActive;
    property Track : Boolean read FTrack write FTrack;
  end;

  TControlBindingState = class(TCustomBindingState)
  public
    destructor Destroy; override;
  end;

  IDestinationBinding = interface
  ['{225A2C76-E6C0-40EC-9396-69150EBE96C8}']
    function Active : IDestinationBinding;
    function Inactive : IDestinationBinding;
    function Track : IDestinationBinding;
  end;

  ISourceBinding = interface
  ['{D16A2933-9497-4E8F-AB39-20B3D350D6D6}']
    function GetBindingState: TCustomBindingState;
    function ToTarget(Destination : TComponent; PropertyName : string): IDestinationBinding;
    property BindingState : TCustomBindingState read GetBindingState;
  end;

  TDestinationBinding = class(TInterfacedObject, IDestinationBinding)
  private
    FBindingState : TCustomBindingState;
  public
    constructor Create(BindingState : TCustomBindingState); reintroduce;
    function Active : IDestinationBinding;
    function Inactive : IDestinationBinding;
    function Track : IDestinationBinding;
  end;


  TSourceBinding = class(TInterfacedObject, ISourceBinding)
  private
    FBindingState : TCustomBindingState;
  protected
    function GetBindingState: TCustomBindingState;
  public
    property BindingState : TCustomBindingState read GetBindingState;
    constructor Create(BindingState : TCustomBindingState); reintroduce;
    destructor Destroy; override;
    function ToTarget(Destination : TComponent; PropertyName : string): IDestinationBinding;
  end;


implementation

{ TCustomBindingState }

constructor TCustomBindingState.Create(BindingsList: TBindingsList);
begin
  FBindingsList := BindingsList;
  FActive := True;
  FTrack := False;
end;

{ TSourceBinding }


function TSourceBinding.ToTarget(
  Destination: TComponent; PropertyName : string): IDestinationBinding;
begin
  FBindingState.Destination := Destination;
  FBindingState.PropertyName := PropertyName;

  Result := TDestinationBinding.Create(FBindingState) as IDestinationBinding;
end;

constructor TSourceBinding.Create(BindingState: TCustomBindingState);
begin
  inherited Create;
  FBindingState := BindingState;
end;

destructor TSourceBinding.Destroy;
begin
  FBindingState.Free;
  inherited;
end;

function TSourceBinding.GetBindingState: TCustomBindingState;
begin
  Result := FBindingState;
end;

{ TDestinationBinding }

function TDestinationBinding.Active : IDestinationBinding;
begin
  FBindingState.Active := True;
  Result := self;
end;

constructor TDestinationBinding.Create(BindingState: TCustomBindingState);
begin
  FBindingState := BindingState;
end;

function TDestinationBinding.Inactive : IDestinationBinding;
begin
  FBindingState.Active := False;
  Result := self;
end;

function TDestinationBinding.Track: IDestinationBinding;
begin
  FBindingState.Track := True;
  Result := self;
end;

{ TControlBindingState }

destructor TControlBindingState.Destroy;
var
  LLink : TLinkControlToProperty;
begin
//  FLinkCOntrolToProperty := TLinkControlToProperty.Create(nil);
//  FLinkCOntrolToProperty.BindingsList := BindingsList1;
//  FLinkCOntrolToProperty.Control := Edit2;
//  FLinkCOntrolToProperty.Component := Label2;
//  FLinkCOntrolToProperty.ComponentProperty := 'Text';
//  FLinkCOntrolToProperty.Active := True;

  LLink := TLinkControlToProperty.Create(nil);
  LLink.BindingsList := FBindingsList;
  LLink.Control := TComponent(FSource);
  LLink.Component := TComponent(FDestination);
  LLink.ComponentProperty := FPropertyName;
  LLink.Track := FTrack;
  LLink.Active := FActive;


  inherited;
end;

end.
