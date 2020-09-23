{****************************************************}
{                                                    }
{  LiveBindings.Fluent                               }
{                                                    }
{  Copyright (C) 2016 Malcolm Groves                 }
{                                                    }
{  http://www.code-partners.com                      }
{                                                    }
{****************************************************}
{                                                    }
{  This Source Code Form is subject to the terms of  }
{  the Mozilla Public License, v. 2.0. If a copy of  }
{  the MPL was not distributed with this file, You   }
{  can obtain one at                                 }
{                                                    }
{  http://mozilla.org/MPL/2.0/                       }
{                                                    }
{****************************************************}
unit LiveBindings.Fluent;

interface
uses
  Data.Bind.Components, Classes, Data.Bind.DBScope, FMX.Grid;

type
  TBindDirection = (TargetToSource, SourceToTarget, Bidirectional);
  TTargetType = (List, Grid, Control);

  IComponentSource = interface
  ['{225A2C76-E6C0-40EC-9396-69150EBE96C8}']
    function Active : IComponentSource;
    function Inactive : IComponentSource;
    function BiDirectional : IComponentSource;
    function FromComponentToSource : IComponentSource;
    function FromSourceToComponent : IComponentSource;
  end;

  IBindSourceSource = interface
  ['{D25D3FE7-9BB1-4E4E-8510-9457762AF067}']
    function Active : IBindSourceSource;
    function Inactive : IBindSourceSource;
  end;

  IFieldSource = interface (IBindSourceSource)
  ['{9FC9A36D-0DE6-482A-BF93-B32BC377335E}']
    function BiDirectional : IFieldSource;
    function FromComponentToData : IFieldSource;
    function FromDataToComponent : IFieldSource;
  end;

  IExpressionSource = interface
  ['{56247E48-C735-4FD8-831E-6AB36C0C3C71}']
    function Active : IExpressionSource;
    function Inactive : IExpressionSource;
    function BiDirectional : IExpressionSource;
    function FromComponentToData : IExpressionSource;
    function FromDataToComponent : IExpressionSource;
  end;

  IComponentTarget = interface
  ['{D16A2933-9497-4E8F-AB39-20B3D350D6D6}']
    function Format(CustomFormat : string) : IComponentTarget;
    function Parse(CustomParse : string) : IComponentTarget;
    function Track : IComponentTarget;
    function ToComponent(Name : TComponent; PropertyName : string): IComponentSource;
    function ToField(Name : TBindSourceDB; Field : String): IFieldSource;
  end;

  IListComponentTarget = interface
  ['{7062E2A8-E5FA-4C51-B312-55D53D68AC07}']
    function Format(CustomFormat : string) : IListComponentTarget;
    function Parse(CustomParse : string) : IListComponentTarget;
    function ToField(Name : TBindSourceDB; Field : string): IFieldSource;
  end;

  IExpressionTarget = interface
  ['{C5F7D86C-C934-440E-BE12-225378356164}']
    function ToExpression(Scope : TComponent; Expression : string): IExpressionSource;
  end;

  IGridTarget = interface
  ['{4B7B4021-E116-4AE0-BD55-C76C4D5E39CF}']
    function DefaultColumnWidth(Width : Integer): IGridTarget;
    function ToBindSource(Name : TBindSourceDB): IBindSourceSource;
  end;


  TBindingsListHelper = class helper for TBindingsList
  public
    function BindComponent(const Target : TComponent; const BindingOwner : TComponent = nil) : IComponentTarget;
    function BindList(const Target : TComponent; const BindingOwner : TComponent = nil) : IListComponentTarget; virtual;
    function BindGrid(const Target : TCustomGrid; const BindingOwner : TComponent = nil) : IGridTarget; virtual;
    function BindExpression(const Scope : TComponent; Expression : string; const BindingOwner : TComponent = nil) : IExpressionTarget; overload; experimental;
  end;

implementation
uses
  Data.Bind.Grid;

type
  TBindingState = class
  private
    FTarget : TObject;
    FSource: TObject;
    FPropertyName: String;
    FActive : Boolean;
    FBindingsList : TBindingsList;
    FBindingOwner : TComponent;
    FTrack: Boolean;
    FField: string;
    FDirection: TBindDirection;
    FTargetExpression: string;
    FSourceExpression: string;
    FFormat: string;
    FParse: string;
    FTargetType: TTargetType;
    FDefaultColumnWidth: Integer;
  public
    constructor Create(BindingsList : TBindingsList; BindingOwner : TComponent); virtual;
    property BindingsList : TBindingsList read FBindingsList;
    property BindingOwner : TComponent read FBindingOwner;
    property Target : TObject read FTarget write FTarget;
    property TargetType : TTargetType read FTargetType write FTargetType;
    property TargetExpression: string read FTargetExpression write FTargetExpression;
    property Source : TObject read FSource write FSource;
    property SourceExpression: string read FSourceExpression write FSourceExpression;
    property PropertyName : String read FPropertyName write FPropertyName;
    property Active : Boolean read FActive write FActive;
    property Track : Boolean read FTrack write FTrack;
    property Field : string read FField write FField;
    property Direction : TBindDirection read FDirection write FDirection;
    property Format: string read FFormat write FFormat;
    property Parse: string read FParse write FParse;
    property DefaultColumnWidth: Integer read FDefaultColumnWidth write FDefaultColumnWidth;
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
    function BiDirectional : IComponentSource;
    function FromComponentToSource : IComponentSource;
    function FromSourceToComponent : IComponentSource;
  end;

  TBindSourceSource = class(TBaseSource, IBindSourceSource)
  public
    destructor Destroy; override;
    function Active : IBindSourceSource;
    function Inactive : IBindSourceSource;
  end;

  TFieldSource = class(TBindSourceSource, IFieldSource)
  public
    destructor Destroy; override;
    function BiDirectional : IFieldSource;
    function FromComponentToData : IFieldSource;
    function FromDataToComponent : IFieldSource;
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
  protected
    FBindingState : TBindingState;
  public
    constructor Create(BindingState : TBindingState); reintroduce;
  end;

  TComponentTarget = class(TBaseTarget, IComponentTarget)
  public
    function Format(CustomFormat : string) : IComponentTarget;
    function Parse(CustomParse : string) : IComponentTarget;
    function Track : IComponentTarget;
    function ToComponent(Name : TComponent; PropertyName : string): IComponentSource;
    function ToField(Name : TBindSourceDB; Field : String): IFieldSource;
  end;

  TListComponentTarget = class(TBaseTarget, IListComponentTarget)
    function Format(CustomFormat : string) : IListComponentTarget;
    function Parse(CustomParse : string) : IListComponentTarget;
    function ToField(Name : TBindSourceDB; Field : string): IFieldSource;
  end;

  TExpressionTarget = class(TBaseTarget, IExpressionTarget)
    function ToExpression(Scope : TComponent; Expression : string): IExpressionSource;
  end;

  TGridTarget = class(TBaseTarget, IGridTarget)
    function DefaultColumnWidth(Width : Integer): IGridTarget;
    function ToBindSource(Name : TBindSourceDB) : IBindSourceSource;
  end;



constructor TBindingState.Create(BindingsList: TBindingsList; BindingOwner : TComponent);
begin
  FBindingsList := BindingsList;
  FActive := True;
  FTrack := False;
  FDirection := TargetToSource;
  FTargetType := Control;
end;

function TComponentTarget.ToComponent(
  Name: TComponent; PropertyName : string): IComponentSource;
begin
  FBindingState.Source := Name;
  FBindingState.PropertyName := PropertyName;

  Result := TComponentSource.Create(FBindingState) as IComponentSource;
end;

function TComponentTarget.Track: IComponentTarget;
begin
  FBindingState.Track := True;
  Result := self;
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
    LLink := TLinkControlToProperty.Create(FBindingState.BindingOwner);
    LLink.BindingsList := FBindingState.BindingsList;
    LLink.Control := TComponent(FBindingState.Target);
    LLink.Component := TComponent(FBindingState.Source);
    LLink.ComponentProperty := FBindingState.PropertyName;
    LLink.Track := FBindingState.Track;
    LLink.CustomFormat := FBindingState.Format;
    LLink.CustomParse := FBindingState.Parse;
    LLink.Active := FBindingState.Active;
  end;

  if (FBindingState.Direction = TBindDirection.SourceToTarget) or (FBindingState.Direction = TBindDirection.Bidirectional) then
  begin
    LLink := TLinkControlToProperty.Create(FBindingState.BindingOwner);
    LLink.BindingsList := FBindingState.BindingsList;
    LLink.Control := TComponent(FBindingState.Source);
    LLink.Component := TComponent(FBindingState.Target);
    LLink.ComponentProperty := FBindingState.PropertyName;
    LLink.Track := FBindingState.Track;
    LLink.CustomFormat := FBindingState.Format;
    LLink.CustomParse := FBindingState.Parse;
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

constructor TBaseTarget.Create(BindingState: TBindingState);
begin
  inherited Create;
  FBindingState := BindingState;
end;


constructor TBaseSource.Create(BindingState: TBindingState);
begin
  FBindingState := BindingState;
end;

destructor TBaseSource.Destroy;
begin
  FBindingState.Free;
  inherited;
end;

function TComponentTarget.Format(CustomFormat: string): IComponentTarget;
begin
  FBindingState.Format := CustomFormat;
  Result := self;
end;

function TComponentTarget.Parse(CustomParse: string): IComponentTarget;
begin
  FBindingState.Parse := CustomParse;
  Result := self;
end;

function TComponentTarget.ToField(Name: TBindSourceDB; Field : String): IFieldSource;
begin
  FBindingState.Source := Name;
  FBindingState.Direction := Bidirectional;
  FBindingState.Field := Field;

  Result := TFieldSource.Create(FBindingState) as IFieldSource;
end;

function TBindSourceSource.Active: IBindSourceSource;
begin
  FBindingState.Active := True;
  Result := self;
end;

destructor TBindSourceSource.Destroy;
var
  LGridLink : TLinkGridToDataSource;
begin
  if FBindingState.TargetType = Grid then
  begin
    LGridLink := TLinkGridToDataSource.Create(FBindingState.BindingOwner);
    LGridLink.BindingsList := FBindingState.BindingsList;
    LGridLink.GridControl := TComponent(FBindingState.Target);
    LGridLink.DataSource := TBindSourceDB(FBindingState.FSource);
    LGridLink.DefaultColumnWidth := FBindingState.DefaultColumnWidth;
//    LGridLink.Columns :=
    LGridLink.Active := FBindingState.Active;
  end;

  inherited;
end;

function TBindSourceSource.Inactive: IBindSourceSource;
begin
  FBindingState.Active := False;
  Result := self;
end;

function TListComponentTarget.Format(
  CustomFormat: string): IListComponentTarget;
begin
  FBindingState.Format := CustomFormat;
  Result := self;
end;

function TListComponentTarget.Parse(CustomParse: string): IListComponentTarget;
begin
  FBindingState.Parse := CustomParse;
  Result := self;
end;

function TListComponentTarget.ToField(Name : TBindSourceDB; Field : string): IFieldSource;
begin
  FBindingState.Source := Name;
  FBindingState.Field := Field;

  Result := TFieldSource.Create(FBindingState) as IFieldSource;
end;

function TBindingsListHelper.BindComponent(const Target: TComponent;
  const BindingOwner : TComponent): IComponentTarget;
var
  LBindingState : TBindingState;
begin
  LBindingState := TBindingState.Create(self, BindingOwner);
  LBindingState.Target := Target;
  LBindingState.TargetType := Control;
  LBindingState.Direction := TargetToSource;

  Result := TComponentTarget.Create(LBindingState) as IComponentTarget;
end;

function TBindingsListHelper.BindExpression(const Scope: TComponent;
  Expression: string; const BindingOwner : TComponent): IExpressionTarget;
var
  LBindingState : TBindingState;
begin
  LBindingState := TBindingState.Create(self, BindingOwner);
  LBindingState.Target := Scope;
  LBindingState.TargetExpression := Expression;
  LBindingState.Direction := TargetToSource;

  Result := TExpressionTarget.Create(LBindingState) as IExpressionTarget;
end;

function TBindingsListHelper.BindGrid(const Target: TCustomGrid;
  const BindingOwner : TComponent): IGridTarget;
var
  LBindingState : TBindingState;
begin
  LBindingState := TBindingState.Create(self, BindingOwner);
  LBindingState.Target := Target;
  LBindingState.TargetType := Grid;
  LBindingState.DefaultColumnWidth := 64;
// Columns?

  Result := TGridTarget.Create(LBindingState) as IGridTarget;
end;

function TBindingsListHelper.BindList(
  const Target: TComponent; const BindingOwner : TComponent): IListComponentTarget;
var
  LBindingState : TBindingState;
begin
  LBindingState := TBindingState.Create(self, BindingOwner);
  LBindingState.Target := Target;
  LBindingState.TargetType := List;
  LBindingState.Direction := Bidirectional;

  Result := TListComponentTarget.Create(LBindingState) as IListComponentTarget;
end;

function TExpressionTarget.ToExpression(Scope: TComponent;
  Expression: string): IExpressionSource;
begin
  FBindingState.Source := Scope;
  FBindingState.SourceExpression := Expression;

  Result := TExpressionSource.Create(FBindingState) as IExpressionSource;
end;

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
  LLink := TBindExpression.Create(FBindingState.BindingOwner);
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

function TGridTarget.DefaultColumnWidth(Width: Integer): IGridTarget;
begin
  FBindingState.DefaultColumnWidth := Width;
  Result := self;
end;

function TGridTarget.ToBindSource(Name: TBindSourceDB): IBindSourceSource;
begin
  FBindingState.Source := Name;

  Result := TBindSourceSource.Create(FBindingState) as IBindSourceSource;
end;

function TFieldSource.BiDirectional: IFieldSource;
begin
  FBindingState.Direction := TBindDirection.Bidirectional;
  Result := self;
end;

destructor TFieldSource.Destroy;
var
  LLink : TLinkControlToField;
  LListLink : TLinkListControlToField;
begin
  if FBindingState.TargetType = List then
  begin
    LListLink := TLinkListControlToField.Create(FBindingState.BindingOwner);
    LListLink.BindingsList := FBindingState.BindingsList;
    LListLink.Control := TComponent(FBindingState.Target);
    LListLink.DataSource := TBindSourceDB(FBindingState.FSource);
    LListLink.FieldName := FBindingState.Field;
    LListLink.CustomFormat := FBindingState.Format;
    LListLink.CustomParse := FBindingState.Parse;
    case FBindingState.Direction of
      TBindDirection.TargetToSource: LListLink.Direction := linkControlToData;
      TBindDirection.SourceToTarget: LListLink.Direction := linkDataToControl;
      TBindDirection.Bidirectional: LListLink.Direction := linkBidirectional;
    end;
    LListLink.Active := FBindingState.Active;
  end
  else if FBindingState.TargetType = Control then
  begin
    LLink := TLinkControlToField.Create(FBindingState.BindingOwner);
    LLink.BindingsList := FBindingState.BindingsList;
    LLink.Control := TComponent(FBindingState.Target);
    LLink.DataSource := TBindSourceDB(FBindingState.FSource);
    LLink.FieldName := FBindingState.Field;
    LLink.CustomFormat := FBindingState.Format;
    LLink.CustomParse := FBindingState.Parse;
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

function TFieldSource.FromComponentToData: IFieldSource;
begin
  FBindingState.Direction := TBindDirection.TargetToSource;
  Result := self;
end;

function TFieldSource.FromDataToComponent: IFieldSource;
begin
  FBindingState.Direction := TBindDirection.SourceToTarget;
  Result := self;
end;

end.
