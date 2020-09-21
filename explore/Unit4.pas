unit Unit4;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  Data.Bind.EngExt, Fmx.Bind.DBEngExt, System.Rtti, System.Bindings.Outputs,
  Fmx.Bind.Editors, Data.Bind.Components, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Edit, Data.Bind.Controls,
  Data.Bind.DBScope, FMX.Layouts, FMX.ListBox, Fmx.Bind.Navigator, Data.DB,
  Datasnap.DBClient, FMX.Colors, FMX.Objects, FMX.Grid.Style, Fmx.Bind.Grid,
  Data.Bind.Grid, FMX.ScrollBox, FMX.Grid;

type
  TForm4 = class(TForm)
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    BindingsList1: TBindingsList;
    LinkControlToPropertyText: TLinkControlToProperty;
    Label2: TLabel;
    Button1: TButton;
    Edit3: TEdit;
    Edit4: TEdit;
    Button2: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    Button3: TButton;
    Edit5: TEdit;
    Button4: TButton;
    Edit6: TEdit;
    EditButton1: TEditButton;
    BindExpression1: TBindExpression;
    Edit7: TEdit;
    CheckBox3: TCheckBox;
    Button6: TButton;
    Edit8: TEdit;
    CheckBox4: TCheckBox;
    Button7: TButton;
    GroupBox1: TGroupBox;
    ClientDataSet1: TClientDataSet;
    ClientDataSet1Firstname: TStringField;
    ClientDataSet1Lastname: TStringField;
    GroupBox2: TGroupBox;
    BindNavigator1: TBindNavigator;
    ListBox1: TListBox;
    Button8: TButton;
    BindSourceDB1: TBindSourceDB;
    Edit9: TEdit;
    Edit10: TEdit;
    Layout1: TLayout;
    Layout2: TLayout;
    GroupBox3: TGroupBox;
    Layout3: TLayout;
    Button15: TButton;
    StringGrid2: TStringGrid;
    Edit11: TEdit;
    Button5: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure EditButton1Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Edit5Change(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button15Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
    { Private declarations }
    FStrVar : string;
  public
    { Public declarations }

  end;

var
  Form4: TForm4;

implementation
uses
  LiveBindings.Fluent(*, LiveBindings.FormatBuilder*) ;

{$R *.fmx}

procedure TForm4.Button15Click(Sender: TObject);
begin
  BindingsList1.BindGrid(StringGrid2)
                 .DefaultColumnWidth(256)
               .ToBindSource(BindSourceDB1);
end;

procedure TForm4.Button1Click(Sender: TObject);
begin
  BindingsList1.BindComponent(Edit2)
                 .Track
               .ToComponent(Label2, 'Text');
end;

procedure TForm4.Button2Click(Sender: TObject);
begin
  BindingsList1.BindComponent(Edit3)
                 .Format('"Foo " + %s')
               .ToComponent(Edit4, 'Text');
end;

procedure TForm4.Button3Click(Sender: TObject);
begin
  BindingsList1.BindComponent(CheckBox1)
                 .Track
               .ToComponent(CheckBox2, 'IsChecked')
                 .BiDirectional;
end;

procedure TForm4.Button4Click(Sender: TObject);
begin
  BindingsList1.BindComponent(Edit5).ToComponent(self, 'FStrVar');
end;

procedure TForm4.Button5Click(Sender: TObject);
begin
  BindingsList1.BindComponent(Edit11).Track.ToComponent(self, 'Caption');
end;

procedure TForm4.Button6Click(Sender: TObject);
begin
  BindingsList1.BindComponent(Edit7).ToComponent(CheckBox3, 'IsChecked');
end;

procedure TForm4.Button7Click(Sender: TObject);
begin
  BindingsList1.BindComponent(CheckBox4).ToComponent(Edit8, 'Text');
end;

procedure TForm4.Button8Click(Sender: TObject);
begin
// setup bindings
  BindingsList1.BindComponent(Edit9)
                 .Format('"Mr " + %s')
               .ToField(BindSourceDB1, 'Firstname');
  BindingsList1.BindComponent(Edit10)
               .ToField(BindSourceDB1, 'Lastname')
                 .FromDataToComponent;
  BindingsList1.BindList(ListBox1).ToField(BindSourceDB1, 'Firstname');
end;

procedure TForm4.Edit5Change(Sender: TObject);
begin
  BindingsList1.Notify(Edit5, 'Text');
end;

procedure TForm4.EditButton1Click(Sender: TObject);
begin
  Edit6.Text := FStrVar;
end;

procedure TForm4.FormShow(Sender: TObject);
begin
  FStrVar := 'Works!';

  //  Create some records
  ClientDataSet1.Active := True;
  ClientDataSet1.Append;
  try
    ClientDataSet1Firstname.AsString := 'Malcolm';
    ClientDataSet1Lastname.AsString := 'Groves';
  finally
    ClientDataSet1.Post;
  end;
  ClientDataSet1.Append;
  try
    ClientDataSet1Firstname.AsString := 'Julie';
    ClientDataSet1Lastname.AsString := 'Kang';
  finally
    ClientDataSet1.Post;
  end;
end;

end.
