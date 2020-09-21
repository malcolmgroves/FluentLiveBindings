program SimpleCodeBindings;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit4 in 'Unit4.pas' {Form4},
//  LiveBindings.FormatBuilder in '..\LiveBindings.FormatBuilder.pas',
  LiveBindings.Fluent in '..\LiveBindings.Fluent.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TForm4, Form4);
  Application.Run;
end.
