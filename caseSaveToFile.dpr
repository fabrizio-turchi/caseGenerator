program caseSaveToFile;

uses
  System.StartUpCopy,
  FMX.Forms,
  caseSaveToFile_main in 'caseSaveToFile_main.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
