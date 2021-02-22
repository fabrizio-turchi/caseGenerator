unit caseGenerator_trace_application;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.DateTimeCtrls, FMX.Calendar, FMX.Edit, FMX.StdCtrls, FMX.Layouts,
  FMX.ListBox, FMX.Controls.Presentation, caseGenerator_util;

type
  TformTraceApplication = class(TForm)
    Label1: TLabel;
    lbAppAccount: TListBox;
    btnClose: TButton;
    btnAddTrace: TButton;
    btnDeleteTrace: TButton;
    btnCancel: TButton;
    btnModifyTrace: TButton;
    Label2: TLabel;
    edAppName: TEdit;
    Label4: TLabel;
    procedure btnAddTraceClick(Sender: TObject);
    procedure btnDeleteTraceClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnModifyTraceClick(Sender: TObject);
    procedure lbAppAccountChange(Sender: TObject);
  private
    FuuidCase: string;
    FpathCase: String;
    procedure SetuuidCase(const Value: string);
    procedure SetpathCase(const Value: String);
    property uuidCase: string read FuuidCase write SetuuidCase;
    property pathCase: String read FpathCase write SetpathCase;
    function prepareTrace(operation: String): String;
    { Private declarations }
  public
    procedure ShowWithParamater(pathCase: String; uuidCase: String);
    { Public declarations }
  end;

var
  formTraceApplication: TformTraceApplication;

implementation

{$R *.fmx}
uses StrUtils;

{ TForm1 }

procedure TformTraceApplication.btnDeleteTraceClick(Sender: TObject);
begin
  lbAppAccount.Items.Delete(lbAppAccount.ItemIndex);
end;


procedure TformTraceApplication.btnModifyTraceClick(Sender: TObject);
begin
  if lbAppAccount.ItemIndex > - 1 then
    lbAppAccount.Items[lbAppAccount.ItemIndex] := prepareTrace('modify');
end;

procedure TformTraceApplication.lbAppAccountChange(Sender: TObject);
var
  line: String;
begin
  if lbAppAccount.ItemIndex > - 1 then
  begin
    line := lbAppAccount.Items[lbAppAccount.ItemIndex];
    edAppName.Text := ExtractField(line, '"uco-core:proposed:appName":"');
  end;
end;

function TformTraceApplication.prepareTrace(operation: String): String;
var
  line, recSep, indent, guidNoBraces: string;
  Uid: TGUID;
  idx: Integer;
begin
  recSep := #30 + #30; // record separator, not printable
  indent := '   ';

  if (Trim(edAppName.Text) = '')  then
    ShowMessage('Application name is missing!')
  else
  begin
    line := '{' + recSep;
    if operation = 'add' then
    begin
      CreateGUID(Uid);
      guidNoBraces := 'kb:' + Copy(GuidToString(Uid), 2, Length(GuidToString(Uid)) - 2);
    end
    else
      guidNoBraces :=  ExtractField(lbAppAccount.Items[lbAppAccount.ItemIndex], '"@id":"');

    line := line + indent + '"@id":"' + guidNoBraces + '", ' + recSep;
    line := line + indent + '"@type":"uco-observable:CyberItem", ' + recSep;
    line := line + indent + '"uco-core:facets":[' + recSep;
    line := line + indent + '{' + recSep;
    line := line + RepeatString(indent, 2) + '"@type":"uco-observable:Application",' + recSep;
    line := line + indent + '"uco-core:proposed:appName":"' + edAppName.Text + '"' + recSep;;
    line := line + indent + '}' + recSep;
    line := line + indent + ']' + recSep + '}';
  end;
  Result := line;

end;

procedure TformTraceApplication.btnCancelClick(Sender: TObject);
begin
  formTraceApplication.Close;
end;

procedure TformTraceApplication.btnCloseClick(Sender: TObject);
var
  fileJSON: TextFile;
  line:string;
  idx: integer;
begin
  AssignFile(fileJSON, FpathCase + FuuidCase + '-traceAPPLICATION.json');
  if lbAppAccount.Items.Count > 0 then
  begin
    idx := 0;
    Rewrite(fileJSON);  // create new file
    WriteLn(fileJSON, '{');
    line := #9 + '"OBJECTS_APPLICATION":[';
    WriteLn(fileJSON, line);

    for idx:= 0 to lbAppAccount.Items.Count - 2 do
      WriteLn(fileJSON, #9#9 + lbAppAccount.Items[idx] + ',');

    WriteLn(fileJSON, #9#9 + lbAppAccount.Items[idx]);
    WriteLn(fileJSON, #9#9 + ']');
    Write(fileJSON,'}');
    CloseFile(fileJSON);
  end
  else
    deleteFile(FpathCase + FuuidCase + '-traceAPP_ACCOUNT.json');

  formTraceApplication.Close;
end;

procedure TformTraceApplication.btnAddTraceClick(Sender: TObject);
begin
    lbAppAccount.Items.Add(prepareTrace('add'));
    edAppName.Text := '';
end;

procedure TformTraceApplication.SetpathCase(const Value: String);
begin
  FpathCase := Value;
end;

procedure TformTraceApplication.SetuuidCase(const Value: string);
begin
  FuuidCase := Value;
end;

procedure TformTraceApplication.ShowWithParamater(pathCase: String; uuidCase: String);
var
  fileJSON: TextFile;
  line, subLine:string;
begin
  SetUuidCase(uuidCase);
  SetPathCase(pathCase);
  //dir := GetCurrentDir;
  // read file JSON uuidCase-traceAPPLICATION.json
  if FileExists(FpathCase + FuuidCase + '-traceAPPLICATION.json') then
  begin
    AssignFile(fileJSON, FpathCase + FuuidCase + '-traceAPPLICATION.json');
    Reset(fileJSON);
    lbAppAccount.Items.Clear;
    while not Eof(fileJSON) do
    begin
      ReadLn(fileJSON, line);
      line := Trim(line);
      if (line = '{') or (line = '}') or  (line = ']') or (AnsiContainsStr(line, 'OBJECTS_'))  then  // first or last line or root element
      else
      begin
        subLine := Copy(line, Length(line), 1); // rule out of comma

        if subLine = ',' then
          line := Copy(line, 1, Length(line) - 1);

        lbAppAccount.Items.Add(line);
      end;
    end;
    CloseFile(fileJSON);
  end;
//  else
//    ShowMessage(dir + uuidCase + '-identity.json' + ' doesn''t exist');

  formTraceApplication.ShowModal;
end;

end.
