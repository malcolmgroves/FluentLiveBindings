unit CodeBindings;

interface
uses
  Data.Bind.Components, Classes, Data.Bind.DBScope;

type
  TBindDirection = (TargetToSource, SourceToTarget, Bidirectional);


  TBindingState = class
  private
    FTarget : TObject;
    FSource: TObject;
    FPropertyName: String;
    FActive : Boolean;
    FBindingsList : TBindingsList;
    FTrack: Boolean;
    FField: string;
    FDirection: TBindDirection;
    FTargetIsList: boolean;
    FTargetExpression: string;
    FSourceExpression: string;
  public
    constructor Create(BindingsList : TBindingsList); virtual;
    property BindingsList : TBindingsList read FBindingsList;
    property Target : TObject read FTarget write FTarget;
    property Source : TObject read FSource write FSource;
    property TargetIsList : boolean read FTargetIsList write FTargetIsList;
    property TargetExpression: string read FTargetExpression write FTargetExpression;
    property SourceExpression: string read FSourceExpression write FSourceExpression;
    property PropertyName : String read FPropertyName write FPropertyName;
    property Active : Boolean read FActive write FActive;
    property Track : Boolean read FTrack write FTrack;
    property Field : string read FField write FField;
    property Direction : TBindDirection read FDirection write FDirection;
  end;

  IComponentSource = interface
  ['{225A2C76-E6C0-40EC-9396-69150EBE96C8}']
    function Active : IComponentSource;
    function Inactive : IComponentSource;
    function Track : IComponentSource;
    function BiDirectional : IComponentSource;
    function FromComponentToSource : IComponentSource;
    function FromSourceToComponent : IComponentSource;
  end;

  IDatasetSource = interface
  ['{D25D3FE7-9BB1-4E4E-8510-9457762AF067}']
    function Active : IDatasetSource;
    function Inactive : IDatasetSource;
    function Track : IDatasetSource;
    function Field(Fieldname : string) : IDatasetSource;
    function BiDirectional : IDatasetSource;
    function FromComponentToData : IDatasetSource;
    function FromDataToComponent : IDatasetSource;
  end;

  IExpressionSource = interface
  ['{56247E48-C735-4FD8-831E-6AB36C0C3C71}']
    function Active : IExpressionSource;
    function Inactive : IExpressionSource;
    function BiDirectional : IExpressionSource;
    function FromComponentToData : IExpressionSource;
    function FromDataToComponent : IExpressionSource;
  end;

  IBaseTarget = interface
  ['{D18910B1-188F-434A-BB74-B171F024F00A}']
    function GetBindingState: TBindingState;
    property BindingState : TBindingState read GetBindingState;
  end;

  IComponentTarget = interface(IBaseTarget)
  ['{D16A2933-9497-4E8F-AB39-20B3D350D6D6}']
    function ToComponent(Name : TComponent; PropertyName : string): IComponentSource;
    function ToDataset(Name : TBindSourceDB): IDataSetSource;
  end;

  IListComponentTarget = interface(IBaseTarget)
  ['{7062E2A8-E5FA-4C51-B312-55D53D68AC07}']
    function ToDataset(Name : TBindSourceDB): IDataSetSource;
  end;

  IExpressionTarget = interface(IBaseTarget)
  ['{C5F7D86C-C934-440E-BE12-225378356164}']
    function ToExpression(Scope : TComponent; Expression : string): IExpressionSource;
  end;

  TBaseSource = class(TInterfacedObject)
  protected
    FBindingState : TBindingState;
  public
    constructor Create(BindingState : TBindingState); reintroduce;
    destructor Destroy; override;
  end;

  TComponentSource = class(TBaseSource, IComponentSource)
  public
    destructor Destroy; override;
    function Active : IComponentSource;
    function Inactive : IComponentSource;
    function Track : IComponentSource;
    function BiDirectional : IComponentSource;
    function FromComponentToSource : IComponentSource;
    function FromSourceToComponent : IComponentSource;
  end;

  TDatasetSource = class(TBaseSource, IDatasetSource)
  public
    destructor Destroy; override;
    function Active : IDatasetSource;
    function Inactive : IDatasetSource;
    function Track : IDatasetSource;
    function Field(Fieldname : string) : IDatasetSource;
    function BiDirectional : IDatasetSource;
    function FromComponentToData : IDatasetSource;
    function FromDataToComponent : IDatasetSource;
  end;

  TExpressionSource = class(TBaseSource, IExpressionSource)
  public
    destructor Destroy; override;
    function Active : IExpressionSource;
    function Inactive : IExpressionSource;
    function BiDirectional : IExpressionSource;
    function FromComponentToData : IExpressionSource;
    function FromDataToComponent : IExpressionSource;
  end;

  TBaseTarget = class(TInterfacedObject)
  private
    FBindingState : TBindingState;
  protected
    function GetBindingState: TBindingState;
  public
    property BindingState : TBindingState read GetBindingState;
    constructor Create(BindingState : TBindingState); reintroduce;
  end;

  TComponentTarget = class(TBaseTarget, IComponentTarget)
  public
    function ToComponent(Name : TComponent; PropertyName : string): IComponentSource;
    function ToDataset(Name : TBindSourceDB): IDataSetSource;
  end;

  TListComponentTarget = class(TBaseTarget, IListComponentTarget)
    function ToDataset(Name : TBindSourceDB): IDataSetSource;
  end;

  TExpressionTarget = class(TBaseTarget, IExpressionTarget)
    function ToExpression(Scope : TComponent; Expression : string): IExpressionSource;
  end;


  TBindingsListHelper = class helper for TBindingsList
    function Bind(const Target : TComponent) : IComponentTarget;  overload;
    function Bind(const Scope : TComponent; Expression : string) : IExpressionTarget; overload;
    function BindList(const Target : TComponent) : IListComponentTarget; virtual;
  end;

implementation


constructor TBindingState.Create(BindingsList: TBindingsList);
begin
  FBindingsList := BindingsList;
  FActive := True;
  FTrack := False;
  FDirection := TargetToSource;
  FTargetIsList := False;
end;

{ TTargetBinding }


function TComponentTarget.ToComponent(
  Name: TComponent; PropertyName : string): IComponentSource;
begin
  FBindingState.Source := Name;
  FBindingState.PropertyName := PropertyName;

  Result := TComponentSource.Create(FBindingState) as IComponentSource;
end;



function TComponentSource.Active : IComponentSource;
begin
  FBindingState.Active := True;
  Result := self;
end;


function TComponentSource.BiDirectional: IComponentSource;
begin
  FBindingState.Direction := TBindDirection.Bidirectional;
  Result := self;
end;

destructor TComponentSource.Destroy;
var
  LLink : TLinkControlToProperty;
begin
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

  inherited;
end;

function TComponentSource.FromComponentToSource: IComponentSource;
begin
  FBindingState.Direction := TBindDirection.TargetToSource;
  Result := self;
end;

function TComponentSource.FromSourceToComponent: IComponentSource;
begin
  FBindingState.Direction := TBindDirection.SourceToTarget;
  Result := self;
end;

function TComponentSource.Inactive : IComponentSource;
begin
  FBindingState.Active := False;
  Result := self;
end;

function TComponentSource.Track: IComponentSource;
begin
  FBindingState.Track := True;
  Result := self;
end;

{ TBaseTarget }

constructor TBaseTarget.Create(BindingState: TBindingState);
begin
  inherited Create;
  FBindingState := BindingState;
end;


function TBaseTarget.GetBindingState: TBindingState;
begin
  Result := FBindingState;
end;

{ TBaseSource }

constructor TBaseSource.Create(BindingState: TBindingState);
begin
  FBindingState := BindingState;
end;

destructor TBaseSource.Destroy;
begin
  FBindingState.Free;
  inherited;
end;

function TComponentTarget.ToDataset(Name: TBindSourceDB): IDataSetSource;
begin
  FBindingState.Source := Name;
  FBindingState.Direction := Bidirectional;

  Result := TDataSetSource.Create(FBindingState) as IDataSetSource;
end;

{ TDatasetSource }

function TDatasetSource.Active: IDatasetSource;
begin
  FBindingState.Active := True;
  Result := self;
end;


function TDatasetSource.BiDirectional: IDatasetSource;
begin
  FBindingState.Direction := TBindDirection.Bidirectional;
  Result := self;
end;

function TDatasetSource.FromComponentToData: IDatasetSource;
begin
  FBindingState.Direction := TBindDirection.TargetToSource;
  Result := self;
end;

destructor TDatasetSource.Destroy;
var
  LLink : TLinkControlToField;
  LListLink : TLinkListControlToField;
begin
  if FBindingState.TargetIsList then
  begin
    LListLink := TLinkListControlToField.Create(nil);
    LListLink.BindingsList := FBindingState.BindingsList;
    LListLink.Control := TComponent(FBindingState.Target);
    LListLink.DataSource := TBindSourceDB(FBindingState.FSource);
    LListLink.FieldName := FBindingState.Field;
    case FBindingState.Direction of
      TBindDirection.TargetToSource: LListLink.Direction := linkControlToData;
      TBindDirection.SourceToTarget: LListLink.Direction := linkDataToControl;
      TBindDirection.Bidirectional: LListLink.Direction := linkBidirectional;
    end;
    LListLink.Active := FBindingState.Active;
  end
  else
  begin
    LLink := TLinkControlToField.Create(nil);
    LLink.BindingsList := FBindingState.BindingsList;
    LLink.Control := TComponent(FBindingState.Target);
    LLink.DataSource := TBindSourceDB(FBindingState.FSource);
    LLink.FieldName := FBindingState.Field;
    case FBindingState.Direction of
      TBindDirection.TargetToSource: LLink.Direction := linkControlToData;
      TBindDirection.SourceToTarget: LLink.Direction := linkDataToControl;
      TBindDirection.Bidirectional: LLink.Direction := linkBidirectional;
    end;
    LLink.Track := FBindingState.Track;
    LLink.Active := FBindingState.Active;
  end;

  inherited;
end;

function TDatasetSource.Field(Fieldname: string): IDatasetSource;
begin
  FBindingState.Field := Fieldname;
  Result := self;
end;

function TDatasetSource.FromDataToComponent: IDatasetSource;
begin
  FBindingState.Direction := TBindDirection.SourceToTarget;
  Result := self;
end;

function TDatasetSource.Inactive: IDatasetSource;
begin
  FBindingState.Active := False;
  Result := self;
end;

function TDatasetSource.Track: IDatasetSource;
begin
  FBindingState.Track := True;
  Result := self;
end;

{ TListComponentTarget }

function TListComponentTarget.ToDataset(Name: TBindSourceDB): IDataSetSource;
begin
  FBindingState.Source := Name;

  Result := TDataSetSource.Create(FBindingState) as IDataSetSource;
end;

{ TBindingsListHelper }

function TBindingsListHelper.Bind(const Target: TComponent): IComponentTarget;
var
  LBindingState : TBindingState;
begin
  LBindingState := TBindingState.Create(self);
  LBindingState.Target := Target;
  LBindingState.Direction := TargetToSource;

  Result := TComponentTarget.Create(LBindingState) as IComponentTarget;
end;

function TBindingsListHelper.Bind(const Scope: TComponent;
  Expression: string): IExpressionTarget;
var
  LBindingState : TBindingState;
begin
  LBindingState := TBindingState.Create(self);
  LBindingState.Target := Scope;
  LBindingState.TargetExpression := Expression;
  LBindingState.Direction := TargetToSource;

  Result := TExpressionTarget.Create(LBindingState) as IExpressionTarget;
end;

function TBindingsListHelper.BindList(
  const Target: TComponent): IListComponentTarget;
var
  LBindingState : TBindingState;
begin
  LBindingState := TBindingState.Create(self);
  LBindingState.Target := Target;
  LBindingState.TargetIsList := True;
  LBindingState.Direction := Bidirectional;

  Result := TListComponentTarget.Create(LBindingState) as IListComponentTarget;
end;

{ TExpressionTarget }

function TExpressionTarget.ToExpression(Scope: TComponent;
  Expression: string): IExpressionSource;
begin
  FBindingState.Source := Scope;
  FBindingState.SourceExpression := Expression;

  Result := TExpressionSource.Create(FBindingState) as IExpressionSource;
end;

{ TExpressionSource }

function TExpressionSource.Active: IExpressionSource;
begin
  FBindingState.Active := True;
  Result := self;
end;

function TExpressionSource.BiDirectional: IExpressionSource;
begin
  FBindingState.Direction := TBindDirection.Bidirectional;
  Result := self;
end;

destructor TExpressionSource.Destroy;
var
  LLink : TBindExpression;
begin
  LLink := TBindExpression.Create(nil);
  LLink.BindingsList := FBindingState.BindingsList;
  LLink.ControlComponent := TComponent(FBindingState.Target);
  LLink.ControlExpression := FBindingState.TargetExpression;
  LLink.SourceComponent := TComponent(FBindingState.Source);
  LLink.SourceExpression := FBindingState.SourceExpression;
  case FBindingState.Direction of
    TBindDirection.TargetToSource: LLink.Direction := dirControlToSource;
    TBindDirection.SourceToTarget: LLink.Direction := dirSourceToControl;
    TBindDirection.Bidirectional: LLink.Direction := dirBidirectional;
  end;
  LLink.Active := FBindingState.Active;

  inherited;
end;

function TExpressionSource.FromComponentToData: IExpressionSource;
begin
  FBindingState.Direction := TBindDirection.TargetToSource;
  Result := self;
end;

function TExpressionSource.FromDataToComponent: IExpressionSource;
begin
  FBindingState.Direction := TBindDirection.SourceToTarget;
  Result := self;
end;

function TExpressionSource.Inactive: IExpressionSource;
begin
  FBindingState.Active := False;
  Result := self;
end;


end.
