unit CodeBindings;

interface
uses
  Data.Bind.Components, Classes, Data.Bind.DBScope;

type
  TBindDirection = (SourceToTarget, TargetToSource, Bidirectional);


  TBindingState = class
  private
    FSource : TObject;
    FTarget: TObject;
    FPropertyName: String;
    FActive : Boolean;
    FBindingsList : TBindingsList;
    FTrack: Boolean;
    FField: string;
    FDirection: TBindDirection;
    FSourceIsList: boolean;
  public
    constructor Create(BindingsList : TBindingsList); virtual;
    property BindingsList : TBindingsList read FBindingsList;
    property Source : TObject read FSource write FSource;
    property Target : TObject read FTarget write FTarget;
    property SourceIsList : boolean read FSourceIsList write FSourceIsList;
    property PropertyName : String read FPropertyName write FPropertyName;
    property Active : Boolean read FActive write FActive;
    property Track : Boolean read FTrack write FTrack;
    property Field : string read FField write FField;
    property Direction : TBindDirection read FDirection write FDirection;
  end;

  IComponentTarget = interface
  ['{225A2C76-E6C0-40EC-9396-69150EBE96C8}']
    function Active : IComponentTarget;
    function Inactive : IComponentTarget;
    function Track : IComponentTarget;
    function BiDirectional : IComponentTarget;
    function FromComponentToTarget : IComponentTarget;
    function FromTargetToComponent : IComponentTarget;
  end;

  IDatasetTarget = interface
  ['{D25D3FE7-9BB1-4E4E-8510-9457762AF067}']
    function Active : IDatasetTarget;
    function Inactive : IDatasetTarget;
    function Track : IDatasetTarget;
    function Field(Fieldname : string) : IDatasetTarget;
    function BiDirectional : IDatasetTarget;
    function FromComponentToData : IDatasetTarget;
    function FromDataToComponent : IDatasetTarget;
  end;

  IBaseSource = interface
  ['{D18910B1-188F-434A-BB74-B171F024F00A}']
    function GetBindingState: TBindingState;
    property BindingState : TBindingState read GetBindingState;
  end;

  IComponentSource = interface(IBaseSource)
  ['{D16A2933-9497-4E8F-AB39-20B3D350D6D6}']
    function ToComponent(Name : TComponent; PropertyName : string): IComponentTarget;
    function ToDataset(Name : TBindSourceDB): IDataSetTarget;
  end;

  IListComponentSource = interface(IBaseSource)
  ['{7062E2A8-E5FA-4C51-B312-55D53D68AC07}']
    function ToDataset(Name : TBindSourceDB): IDataSetTarget;
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
    function BiDirectional : IComponentTarget;
    function FromComponentToTarget : IComponentTarget;
    function FromTargetToComponent : IComponentTarget;
  end;

  TDatasetTarget = class(TBaseTarget, IDatasetTarget)
  public
    destructor Destroy; override;
    function Active : IDatasetTarget;
    function Inactive : IDatasetTarget;
    function Track : IDatasetTarget;
    function Field(Fieldname : string) : IDatasetTarget;
    function BiDirectional : IDatasetTarget;
    function FromComponentToData : IDatasetTarget;
    function FromDataToComponent : IDatasetTarget;
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
    function ToDataset(Name : TBindSourceDB): IDataSetTarget;
  end;

  TListComponentSource = class(TBaseSource, IListComponentSource)
    function ToDataset(Name : TBindSourceDB): IDataSetTarget;
  end;


  TBindingsListHelper = class helper for TBindingsList
    function Bind(const Source : TComponent) : IComponentSource; virtual;
    function BindList(const Source : TComponent) : IListComponentSource; virtual;
  end;

implementation


constructor TBindingState.Create(BindingsList: TBindingsList);
begin
  FBindingsList := BindingsList;
  FActive := True;
  FTrack := False;
  FDirection := SourceToTarget;
  FSourceIsList := False;
end;

{ TSourceBinding }


function TComponentSource.ToComponent(
  Name: TComponent; PropertyName : string): IComponentTarget;
begin
  FBindingState.Target := Name;
  FBindingState.PropertyName := PropertyName;

  Result := TComponentTarget.Create(FBindingState) as IComponentTarget;
end;



function TComponentTarget.Active : IComponentTarget;
begin
  FBindingState.Active := True;
  Result := self;
end;


function TComponentTarget.BiDirectional: IComponentTarget;
begin
  FBindingState.Direction := TBindDirection.Bidirectional;
  Result := self;
end;

destructor TComponentTarget.Destroy;
var
  LLink : TLinkControlToProperty;
begin
  if (FBindingState.Direction = TBindDirection.SourceToTarget) or (FBindingState.Direction = TBindDirection.Bidirectional) then
  begin
    LLink := TLinkControlToProperty.Create(nil);
    LLink.BindingsList := FBindingState.BindingsList;
    LLink.Control := TComponent(FBindingState.Source);
    LLink.Component := TComponent(FBindingState.Target);
    LLink.ComponentProperty := FBindingState.PropertyName;
    LLink.Track := FBindingState.Track;
    LLink.Active := FBindingState.Active;
  end;

  if (FBindingState.Direction = TBindDirection.TargetToSource) or (FBindingState.Direction = TBindDirection.Bidirectional) then
  begin
    LLink := TLinkControlToProperty.Create(nil);
    LLink.BindingsList := FBindingState.BindingsList;
    LLink.Control := TComponent(FBindingState.Target);
    LLink.Component := TComponent(FBindingState.Source);
    LLink.ComponentProperty := FBindingState.PropertyName;
    LLink.Track := FBindingState.Track;
    LLink.Active := FBindingState.Active;
  end;

  inherited;
end;

function TComponentTarget.FromComponentToTarget: IComponentTarget;
begin
  FBindingState.Direction := TBindDirection.SourceToTarget;
  Result := self;
end;

function TComponentTarget.FromTargetToComponent: IComponentTarget;
begin
  FBindingState.Direction := TBindDirection.TargetToSource;
  Result := self;
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

function TComponentSource.ToDataset(Name: TBindSourceDB): IDataSetTarget;
begin
  FBindingState.Target := Name;
  FBindingState.Direction := Bidirectional;

  Result := TDataSetTarget.Create(FBindingState) as IDataSetTarget;
end;

{ TDatasetTarget }

function TDatasetTarget.Active: IDatasetTarget;
begin
  FBindingState.Active := True;
  Result := self;
end;


function TDatasetTarget.BiDirectional: IDatasetTarget;
begin
  FBindingState.Direction := TBindDirection.Bidirectional;
  Result := self;
end;

function TDatasetTarget.FromComponentToData: IDatasetTarget;
begin
  FBindingState.Direction := TBindDirection.SourceToTarget;
  Result := self;
end;

destructor TDatasetTarget.Destroy;
var
  LLink : TLinkControlToField;
  LListLink : TLinkListControlToField;
begin
  if FBindingState.SourceIsList then
  begin
    LListLink := TLinkListControlToField.Create(nil);
    LListLink.BindingsList := FBindingState.BindingsList;
    LListLink.Control := TComponent(FBindingState.Source);
    LListLink.DataSource := TBindSourceDB(FBindingState.FTarget);
    LListLink.FieldName := FBindingState.Field;
    case FBindingState.Direction of
      TBindDirection.SourceToTarget: LListLink.Direction := linkControlToData;
      TBindDirection.TargetToSource: LListLink.Direction := linkDataToControl;
      TBindDirection.Bidirectional: LListLink.Direction := linkBidirectional;
    end;
    LListLink.Active := FBindingState.Active;
  end
  else
  begin
    LLink := TLinkControlToField.Create(nil);
    LLink.BindingsList := FBindingState.BindingsList;
    LLink.Control := TComponent(FBindingState.Source);
    LLink.DataSource := TBindSourceDB(FBindingState.FTarget);
    LLink.FieldName := FBindingState.Field;
    case FBindingState.Direction of
      TBindDirection.SourceToTarget: LLink.Direction := linkControlToData;
      TBindDirection.TargetToSource: LLink.Direction := linkDataToControl;
      TBindDirection.Bidirectional: LLink.Direction := linkBidirectional;
    end;
    LLink.Track := FBindingState.Track;
    LLink.Active := FBindingState.Active;
  end;

  inherited;
end;

function TDatasetTarget.Field(Fieldname: string): IDatasetTarget;
begin
  FBindingState.Field := Fieldname;
  Result := self;
end;

function TDatasetTarget.FromDataToComponent: IDatasetTarget;
begin
  FBindingState.Direction := TBindDirection.TargetToSource;
  Result := self;
end;

function TDatasetTarget.Inactive: IDatasetTarget;
begin
  FBindingState.Active := False;
  Result := self;
end;

function TDatasetTarget.Track: IDatasetTarget;
begin
  FBindingState.Track := True;
  Result := self;
end;

{ TListComponentSource }

function TListComponentSource.ToDataset(Name: TBindSourceDB): IDataSetTarget;
begin
  FBindingState.Target := Name;

  Result := TDataSetTarget.Create(FBindingState) as IDataSetTarget;
end;

{ TBindingsListHelper }

function TBindingsListHelper.Bind(const Source: TComponent): IComponentSource;
var
  LBindingState : TBindingState;
begin
  LBindingState := TBindingState.Create(self);
  LBindingState.Source := Source;
  LBindingState.Direction := SourceToTarget;

  Result := TComponentSource.Create(LBindingState) as IComponentSource;
end;

function TBindingsListHelper.BindList(
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
