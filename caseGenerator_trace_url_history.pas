unit caseGenerator_trace_url_history;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.DateTimeCtrls, FMX.Calendar, FMX.Edit, FMX.StdCtrls, FMX.Layouts,
  FMX.ListBox, FMX.Controls.Presentation, caseGenerator_util;

type
  TformTraceUrlHistory = class(TForm)
    Label1: TLabel;
    lbURLs: TListBox;
    btnClose: TButton;
    btnAddTrace: TButton;
    btnDeleteTrace: TButton;
    btnCancel: TButton;
    btnModifyTrace: TButton;
    edBrowser: TEdit;
    Label4: TLabel;
    Label2: TLabel;
    edURL: TEdit;
    cbFirstVisitDay: TComboBox;
    cbFirstVisitMonth: TComboBox;
    cbFirstVisitYear: TComboBox;
    timeFirstVisit: TTimeEdit;
    Label3: TLabel;
    Label5: TLabel;
    cbLastVisitDay: TComboBox;
    cbLastVisitMonth: TComboBox;
    cbLastVisitYear: TComboBox;
    timeLastVisit: TTimeEdit;
    Label6: TLabel;
    edTitle: TEdit;
    Label7: TLabel;
    edVisitCount: TEdit;
    lbUrlAddress: TListBox;
    procedure btnAddTraceClick(Sender: TObject);
    procedure btnDeleteTraceClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnModifyTraceClick(Sender: TObject);
    procedure lbURLsChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FuuidCase: string;
    FpathCase: String;
    procedure SetuuidCase(const Value: string);
    procedure SetpathCase(const Value: String);
    property uuidCase: string read FuuidCase write SetuuidCase;
    property pathCase: String read FpathCase write SetpathCase;
    function prepareTrace(operation: String): String;
    function prepareTraceURL(url: String; newUuid: Boolean): String;
    { Private declarations }
  public
    procedure ShowWithParamater(pathCase: String; uuidCase: String);
    { Public declarations }
  end;

var
  formTraceUrlHistory: TformTraceUrlHistory;

implementation

{$R *.fmx}
uses StrUtils;

{ TForm1 }

procedure TformTraceUrlHistory.btnDeleteTraceClick(Sender: TObject);
begin
  lbURLs.Items.Delete(lbURLs.ItemIndex);
end;


procedure TformTraceUrlHistory.btnModifyTraceClick(Sender: TObject);
begin
  if lbURLs.ItemIndex > - 1 then
    lbURLs.Items[lbURLs.ItemIndex] := prepareTrace('modify');
end;

procedure TformTraceUrlHistory.FormShow(Sender: TObject);
var
  idx, idy: Integer;
begin
  idy := 0;
  for idx := 2020 to 2030 do
  begin
    cbFirstVisitYear.Items.Add(IntToStr(idx));
    if (IntToStr(CurrentYear) = IntToStr(idx)) then
      cbFirstVisitYear.ItemIndex := idy;
    Inc(idy);
  end;

  idy := 0;
  for idx := 2020 to 2030 do
  begin
    cbLastVisitYear.Items.Add(IntToStr(idx));
    if (IntToStr(CurrentYear) = IntToStr(idx)) then
      cbLastVisitYear.ItemIndex := idy;
    Inc(idy);
  end;

  timeFirstVisit.Text := '09:00:00';
  timeLastVisit.Text := '11:00:00';
end;

procedure TformTraceUrlHistory.lbURLsChange(Sender: TObject);
var
  line, urlAddress, field, sDate, sDay, sMonth, sYear: String;
  idx: Integer;
begin
  if lbURLs.ItemIndex > - 1 then
  begin
    line := lbURLs.Items[lbURLs.ItemIndex];
    edBrowser.Text := ExtractField(line, '"uco-observable:browserInformation":"');
    urlAddress := lbUrlAddress.Items[lbURLs.ItemIndex];
    edUrl.Text := ExtractField(urlAddress, '"uco-observable:fullValue":"');
    field := ExtractDateValue(line, '"uco-observable:firstVisit":');
    sDate := Copy(field, 1, 10);
    sDay := Copy(sDate, 7, 2);
    for idx:=0 to cbFirstVisitDay.Items.Count - 1 do
    begin
      if cbFirstVisitDay.Items[idx] = sDay then
      begin
        cbFirstVisitDay.ItemIndex := idx;
        break;
      end;
    end;

    sMonth := Copy(sDate, 5, 2);
    for idx:=0 to cbFirstVisitMonth.Items.Count - 1 do
    begin
      if cbFirstVisitMonth.Items[idx] = sMonth then
      begin
        cbFirstVisitMonth.ItemIndex := idx;
        break;
      end;
    end;

    sYear := Copy(sDate, 1, 4);
    for idx:=0 to cbFirstVisitYear.Items.Count - 1 do
    begin
      if cbFirstVisitYear.Items[idx] = sYear then
      begin
        cbFirstVisitYear.ItemIndex := idx;
        break;
      end;
    end;

    timeFirstVisit.Text := Copy(field, 10, 8);

    field := ExtractDateValue(line, '"uco-observable:lastVisit":');
    sDate := Copy(field, 1, 10);
    sDay := Copy(sDate, 7, 2);
    for idx:=0 to cbLastVisitDay.Items.Count - 1 do
    begin
      if cbLastVisitDay.Items[idx] = sDay then
      begin
        cbLastVisitDay.ItemIndex := idx;
        break;
      end;
    end;

    sMonth := Copy(sDate, 5, 2);
    for idx:=0 to cbLastVisitMonth.Items.Count - 1 do
    begin
      if cbLastVisitMonth.Items[idx] = sMonth then
      begin
        cbLastVisitMonth.ItemIndex := idx;
        break;
      end;
    end;

    sYear := Copy(sDate, 1, 4);
    for idx:=0 to cbLastVisitYear.Items.Count - 1 do
    begin
      if cbLastVisitMonth.Items[idx] = sYear then
      begin
        cbLastVisitMonth.ItemIndex := idx;
        break;
      end;
    end;

    timeLastVisit.Text := Copy(field, 10, 8);

    edTitle.Text := ExtractField(line, '"uco-observable:pageTitle":"');
    edVisitCount.Text := ExtractField(line, '"uco-observable:visitCount":"');

  end;
end;


function TformTraceUrlHistory.prepareTrace(operation: String): String;
var
  line, recSep, indent, guidNoBraces, uuidURL: string;
  Uid: TGUID;
  idx: Integer;
begin
  recSep := #30 + #30; // record separator, not printable
  indent := '   ';

  if (Trim(edURL.Text) = '')  then
    ShowMessage('URL is missing!')
  else
  begin
    line := '{' + recSep;
    if operation = 'add' then
    begin
      CreateGUID(Uid);
      guidNoBraces := 'kb:' + Copy(GuidToString(Uid), 2, Length(GuidToString(Uid)) - 2);
      uuidURL := prepareTraceURL(edURL.Text, True);
    end
    else
    begin
      guidNoBraces :=  ExtractField(lbURLs.Items[lbURLs.ItemIndex], '"@id":"');
      uuidURL :=     ExtractField(lbUrlAddress.Items[lbURLs.ItemIndex], '@id');
    end;

    line := line + indent + '"@id":"' + guidNoBraces + '", ' + recSep;
    line := line + indent + '"@type":"uco-observable:CyberItem", ' + recSep;
    line := line + indent + '"uco-core:facets":[' + recSep;
    line := line + indent + '{' + recSep;
    line := line + RepeatString(indent, 2) + '"@type":"uco-observable:URLHistory",' + recSep;
    line := line + RepeatString(indent, 2) + '"uco-observable:browserInformation":"' + edBrowser.Text + '",' + recSep;;
    line := line + RepeatString(indent, 2) + '"uco-observable:urlHistoryEntry": [' + recSep;;
    line := line + RepeatString(indent, 2) + '{' + recSep;
    line := line + RepeatString(indent, 3) + '"uco-observable:firstVisit":' + recSep;
    line := line + RepeatString(indent, 3) + '{' + recSep;
    line := line + RepeatString(indent, 4) + '"@type":"xsd:dateTime",' + recSep;
    line := line + RepeatString(indent, 4) + '"@value":"' + cbFirstVisitYear.Items[cbFirstVisitYear.ItemIndex];
    line := line + cbFirstVisitMonth.Items[cbFirstVisitMonth.ItemIndex];
    line := line + cbFirstVisitDay.Items[cbFirstVisitDay.ItemIndex] + 'T';
    line := line + timeFirstVisit.Text + '"' + recSep;
    line := line +  RepeatString(indent, 3)  + '},' + recSep;
    line := line + RepeatString(indent, 3) + '"uco-observable:lastVisit":' + recSep;
    line := line + RepeatString(indent, 3) + '{' + recSep;
    line := line + RepeatString(indent, 4) + '"@type":"xsd:dateTime",' + recSep;
    line := line + RepeatString(indent, 4) + '"@value":"' + cbLastVisitYear.Items[cbLastVisitYear.ItemIndex];
    line := line + cbLastVisitMonth.Items[cbLastVisitMonth.ItemIndex];
    line := line + cbLastVisitDay.Items[cbLastVisitDay.ItemIndex] + 'T';
    line := line + timeLastVisit.Text + '"' + recSep;
    line := line +  RepeatString(indent, 3)  + '},' + recSep;
    line := line + RepeatString(indent, 3) + '"uco-observable:expiration":"NOT_PROVIDED"' + recSep;
    line := line + RepeatString(indent, 3) + '"uco-observable:userProfile":"NOT_PROVIDED"' + recSep;
    line := line + RepeatString(indent, 3) + '"uco-observable:url":{' + recSep;
    line := line + RepeatString(indent, 4) + '"@id":"' + uuidURL + '"' + recSep;
    line := line +  RepeatString(indent, 3)  + '},' + recSep;
    line := line +  RepeatString(indent, 3)  + '"uco-observable:referrerUrl":"NOT_PROVIDED",' + recSep;
    line := line +  RepeatString(indent, 3)  + '"uco-observable:pageTitle":"' + edTitle.Text + '",' + recSep;
    line := line +  RepeatString(indent, 3)  + '"uco-observable:visitCount":"' + edVisitCount.Text + '",' + recSep;
    line := line +  RepeatString(indent, 3)  + '"uco-observable:manuallyEnteredCount":0,' + recSep;
    line := line +  RepeatString(indent, 3)  + '"uco-observable:keywordSearchTerm":"",' + recSep;
    line := line +  RepeatString(indent, 3)  + '"uco-observable:proposed:allocationStatus":"Intact"' + recSep;
    line := line + indent + '}' + recSep;
    line := line + indent + ']' + recSep + '}';
  end;
  Result := line;

end;

function TformTraceUrlHistory.prepareTraceURL(url: String; newUuid: Boolean): String;
var
  line, recSep, indent, guidNoBraces: string;
  Uid: TGUID;
  idx: Integer;
begin

  if newUuid then
  begin
    CreateGUID(Uid);
    guidNoBraces := 'kb:' + Copy(GuidToString(Uid), 2, Length(GuidToString(Uid)) - 2);
    recSep := #30 + #30; // record separator, not printable
    indent := '   ';
    line := '{' + recSep;
    line := line + indent + '"@id":"' + guidNoBraces + '", ' + recSep;
    line := line + indent + '"@type":"uco-observable:CyberItem", ' + recSep;
    line := line + indent + '"uco-core:facets":[' + recSep;
    line := line + indent + '{' + recSep;
    line := line + RepeatString(indent, 2) + '"@type":"uco-observable:URL",' + recSep;
    line := line + RepeatString(indent, 2) + '"uco-observable:fullValue":"' + url + '"' + recSep;;
    line := line + indent + '}' + recSep;
    line := line + indent + ']' + recSep + '}';
    lbUrlAddress.Items.Add(line);
  end
  else
    guidNoBraces := ExtractField(lbUrlAddress.Items[lbURLs.ItemIndex], '@id');

  Result := guidNoBraces;

end;

procedure TformTraceUrlHistory.btnCancelClick(Sender: TObject);
begin
  formTraceUrlHistory.Close;
end;

procedure TformTraceUrlHistory.btnCloseClick(Sender: TObject);
var
  fileJSON: TextFile;
  line:string;
  idx: integer;
begin
  AssignFile(fileJSON, FpathCase + FuuidCase + '-traceURL_HISTORY.json');
  if lbURLs.Items.Count > 0 then
  begin
    idx := 0;
    Rewrite(fileJSON);  // create new file
    WriteLn(fileJSON, '{');
    line := #9 + '"OBJECTS_URL_HISTORY":[';
    WriteLn(fileJSON, line);

    for idx:= 0 to lbURLs.Items.Count - 2 do
      WriteLn(fileJSON, #9#9 + lbURLs.Items[idx] + ',');

    WriteLn(fileJSON, #9#9 + lbURLs.Items[idx]);
    WriteLn(fileJSON, #9#9 + ']');
    Write(fileJSON,'}');
    CloseFile(fileJSON);

    AssignFile(fileJSON, FpathCase + FuuidCase + '-traceURL_ADDRESS.json');
    idx := 0;
    Rewrite(fileJSON);  // create new file
    WriteLn(fileJSON, '{');
    line := #9 + '"OBJECTS_URL_HISTORY":[';
    WriteLn(fileJSON, line);

    for idx:= 0 to lbUrlAddress.Items.Count - 2 do
      WriteLn(fileJSON, #9#9 + lbUrlAddress.Items[idx] + ',');

    WriteLn(fileJSON, #9#9 + lbUrlAddress.Items[idx]);
    WriteLn(fileJSON, #9#9 + ']');
    Write(fileJSON,'}');
    CloseFile(fileJSON);

  end
  else
  begin
    deleteFile(FpathCase + FuuidCase + '-traceURL_HISTORY.json');
    deleteFile(FpathCase + FuuidCase + '-traceURL_ADDRESS.json');
  end;

  formTraceUrlHistory.Close;
end;

procedure TformTraceUrlHistory.btnAddTraceClick(Sender: TObject);
begin
    lbURLs.Items.Add(prepareTrace('add'));
    edBrowser.Text := '';
    edURL.Text := '';
    edTitle.Text := '';
    edVisitCount.Text := '';
end;

procedure TformTraceUrlHistory.SetpathCase(const Value: String);
begin
  FpathCase := Value;
end;

procedure TformTraceUrlHistory.SetuuidCase(const Value: string);
begin
  FuuidCase := Value;
end;

procedure TformTraceUrlHistory.ShowWithParamater(pathCase: String; uuidCase: String);
var
  fileJSON: TextFile;
  line, subLine:string;
begin
  SetUuidCase(uuidCase);
  SetPathCase(pathCase);
  lbUrlAddress.Visible := False;
  lbUrlAddress.Items.Clear;
  //dir := GetCurrentDir;
  // read file JSON uuidCase-traceAPPLICATION.json
  if FileExists(FpathCase + FuuidCase + '-traceURL_HISTORY.json') then
  begin
    AssignFile(fileJSON, FpathCase + FuuidCase + '-traceURL_HISTORY.json');
    Reset(fileJSON);
    lbURLs.Items.Clear;
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

        lbURLs.Items.Add(line);
      end;
    end;
    CloseFile(fileJSON);
  end;

  if FileExists(FpathCase + FuuidCase + '-traceURL_ADDRESS.json') then
  begin
    AssignFile(fileJSON, FpathCase + FuuidCase + '-traceURL_ADDRESS.json');
    Reset(fileJSON);
    lbUrlAddress.Items.Clear;
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

        lbUrlAddress.Items.Add(line);
      end;
    end;
    CloseFile(fileJSON);
  end;
//  else
//    ShowMessage(dir + uuidCase + '-identity.json' + ' doesn''t exist');

  formTraceUrlHistory.ShowModal;
end;

end.
