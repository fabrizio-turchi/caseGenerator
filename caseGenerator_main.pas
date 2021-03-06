unit caseGenerator_main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.TreeView, System.JSON, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Edit,
  FMX.ListBox, FMX.ScrollBox, FMX.Memo, StrUtils, IOUtils, caseGenerator_util,
  FMX.Memo.Types;

type
  TformMain = class(TForm)
    btnNewCase: TButton;
    Label1: TLabel;
    btnInvestigativeAction: TButton;
    btnIdentity: TButton;
    btnLocation: TButton;
    btnRole: TButton;
    btnTool: TButton;
    OpenDialog1: TOpenDialog;
    btnGenerateJSON: TButton;
    memoJSON: TMemo;
    btnSaveToJSON: TButton;
    btnTrace: TButton;
    cbTrace: TComboBox;
    btnRelationship: TButton;
    btnPR: TButton;
    cbCases: TComboBox;
    lbObjects: TListBox;
    btnWarrant: TButton;
    btnGenerateCaseJson: TButton;
    panelCase: TPanel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    edName: TEdit;
    Label5: TLabel;
    edFocus: TEdit;
    Label6: TLabel;
    memoShortDescription: TMemo;
    Label7: TLabel;
    memoDescription: TMemo;
    btnTimeline: TButton;
    SaveDialog: TSaveDialog;
    btnModify: TButton;
    cbNewOntology: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure btnNewCaseClick(Sender: TObject);
    procedure btnIdentityClick(Sender: TObject);
    procedure btnLocationClick(Sender: TObject);
    procedure btnRoleClick(Sender: TObject);
    procedure btnToolClick(Sender: TObject);
    procedure btnGenerateJSONClick(Sender: TObject);
    procedure btnSaveToJSONClick(Sender: TObject);
    procedure btnTraceClick(Sender: TObject);
    procedure btnRelationshipClick(Sender: TObject);
    procedure btnInvestigativeActionClick(Sender: TObject);
    procedure btnPRClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnWriteTtreeClick(Sender: TObject);
    procedure btnSelectCaseClick(Sender: TObject);
    procedure btnWarrantClick(Sender: TObject);
    procedure btnGenerateCaseJsonClick(Sender: TObject);
    procedure btnExtendCaseClick(Sender: TObject);
    procedure cbCasesChange(Sender: TObject);
    procedure btnTimelineClick(Sender: TObject);
    procedure btnModifyClick(Sender: TObject);
  private
    FuuidCase: String;
    FPathCase: String;
    FHomeCases: String;
    procedure addRootChildren(Sender:TObject; uuidGenerated: String);{ Private declarations }
    procedure addRootCase(Sender:TObject);
    procedure addChildren(itemParent: TTreeViewItem; itemText: String);
    function generateUUID(Sender: TObject): String;
    procedure SetuuidCase(const Value: String);
    procedure ExtractAllItemsFromTreeView(tvNode: TTreeViewItem);
    procedure WriteObjectCASE;
    function  ExtractField(line: String; subLine: String): String;
    procedure SetHomeCases(const Value: String);
    procedure SetPathCase(const Value: String);
    procedure ExtractAllFiles(path: String);
  public
    property uuidCase: String read FuuidCase write SetuuidCase;
    property homeCases:String read FHomeCases write SetHomeCases;
    property pathCase: String read FPathCase write SetPathCase;
    { Public declarations }
  end;

var
  formMain: TformMain;

implementation

uses
  caseGenerator_identity, caseGenerator_location,
  caseGenerator_role, caseGenerator_tool, caseGenerator_trace_mobile,
  caseGenerator_trace_SIM, caseGenerator_trace_computer, caseGenerator_relationship,
  caseGenerator_investigative_action, caseGenerator_trace_file,
  caseGenerator_provenance_record, caseGenerator_trace_phone_account,
  caseGenerator_trace_Phone_Call, caseGenerator_trace_sms, caseGenerator_trace_disk_partition,
  caseGenerator_warrant, caseGenerator_overview, caseGenerator_trace_email_account,
  caseGenerator_trace_email, caseGenerator_GeneralData, caseGenerator_trace_mobile_account,
  caseGenerator_trace_disk, caseGenerator_trace_application, caseGenerator_trace_url_history,
  caseGenerator_trace_application_account, caseGenerator_trace_chat;
{$R *.fmx}

procedure TformMain.addRootChildren(Sender: TObject; uuidGenerated: String);
var
  itemRoot: TTreeViewItem;
  itemContext, itemGeneric: TTreeViewItem;
  itemIdx, idx : Integer;
  recSep, space4, space8, line: String;
begin
  recSep := #30 + #30;
  space4 := '    ';
  space8 := space4 + space4;
  lbObjects.Items.Add(space4 + '"@context":{' + recSep);
  lbObjects.Items.Add(space8 + '"@vocab": "http://caseontology.org/core#",' + recSep);
  lbObjects.Items.Add(space8 + '"case-investigation": "https://caseontology.org/ontology/case/investigation#",' + recSep);
  lbObjects.Items.Add(space8 + '"rdf":"http://www.w3.org/1999/02/22-rdf-syntax-ns#",' + recSep);
  lbObjects.Items.Add(space8 + '"rdfs":"http://www.w3.org/2000/01/rdf-schema#",' + recSep);
  lbObjects.Items.Add(space8 + '"uco-action": "https://unifiedcyberontology.org/ontology/uco/action#",' + recSep);
  lbObjects.Items.Add(space8 + '"uco-core": "https://unifiedcyberontology.org/ontology/uco/core#",' + recSep);
  lbObjects.Items.Add(space8 + '"uco-observable": "https://unifiedcyberontology.org/ontology/uco/observable#",' + recSep);
  lbObjects.Items.Add(space8 + '"uco-identity": "https://unifiedcyberontology.org/ontology/uco/identity#",' + recSep);
  lbObjects.Items.Add(space8 + '"uco-location": "https://unifiedcyberontology.org/ontology/uco/location#",' + recSep);
  lbObjects.Items.Add(space8 + '"uco-tool": "https://unifiedcyberontology.org/ontology/uco/tool#",' + recSep);
  lbObjects.Items.Add(space8 + '"uco-types": "https://unifiedcyberontology.org/ontology/uco/types#",' + recSep);
  lbObjects.Items.Add(space8 + '"uco-vocabulary": "https://unifiedcyberontology.org/ontology/uco/vocabulary#",' + recSep);
  lbObjects.Items.Add(space8 + '"xsd":"http://www.w3.org/2001/XMLSchema#"},' + recSep);
  lbObjects.Items.Add(space4 + '"@id":"bundle-' + uuidGenerated + '",' + recSep);
  lbObjects.Items.Add(space4 + '"@type":"uco-core:Bundle",' + recSep);
  lbObjects.Items.Add(space8 + '"uco-core:__caseNationalNumber":"' + edName.Text + '",' + recSep);
  lbObjects.Items.Add(space8 + '"uco-core:focus":"' + edFocus.Text + '",' + recSep);
  lbObjects.Items.Add(space8 + '"uco-core:name":"' + memoShortDescription.Text + '",' + recSep);
  lbObjects.Items.Add(space8 + '"uco-core:description":"' + memoDescription.Text + '",' + recSep);
  lbObjects.Items.Add(space8 + '"uco-core:object":[' + recSep);
end;

procedure TformMain.addRootCase(Sender: TObject);
var
  itemRoot: TTreeViewItem;
begin
  {itemRoot := TTreeViewItem.Create(nil);
  itemRoot.Text := '[ CASE - ' + formNewCase.editName.Text + ' ]';
  tvObjects.InsertObject(0, itemRoot);}
  lbObjects.Items.Add('{"__CASE__":"' + edName.Text + '", ' + #30 + #30);
end;

procedure TformMain.addChildren(itemParent: TTreeViewItem; itemText: String);
var
  item: TTreeViewItem;
begin
  item := TTreeViewItem.Create(nil);
  item.Text := itemText;
  item.Parent := itemParent;

  //item.Free;
end;


procedure TformMain.btnLocationClick(Sender: TObject);
begin
// debub I put the fix value to 43E9FE90-F5DC-47C4-95F6-3C5BD6DFAA77
//  formIdentity.ShowWithParamater(uuidCase);
  if lbObjects.Items.Count = 0 then
    ShowMessage('Select a case or create a new case')
  else
    formLocation.ShowWithParamater(FHomeCases + FPathCase, FuuidCase);
end;

procedure TformMain.btnModifyClick(Sender: TObject);
var
  operation, idx, nDescriptionMatches : Integer;
  fieldValue: String;
begin
  if cbCases.ItemIndex > - 1 then
  begin
    formNewCase.edName.Text := edName.Text;
    formNewCase.edFocus.Text := edFocus.Text;
    formNewCase.memoShortDescription.Lines := memoShortDescription.Lines;
    formNewCase.memoDescription.Lines := memoDescription.Lines;
    operation := formNewCase.ShowWithParamater();
    if operation > 0 then
    begin
      edName.Text := formNewCase.edName.Text;
      edFocus.Text := formNewCase.edFocus.Text;
      memoShortDescription.Lines := formNewCase.memoShortDescription.Lines;
      memoDescription.Lines := formNewCase.memoDescription.Lines;
      nDescriptionMatches := 0;
  // insert of the main data onf the case
      for idx:=0 to lbObjects.Items.Count - 1 do
      begin
        if AnsiContainsStr(lbObjects.Items[idx], '"description":"') then
        begin
          Inc(nDescriptionMatches);
          fieldValue := ExtractField(lbObjects.Items[idx], '"description":"');
          if nDescriptionMatches < 2 then
            lbObjects.Items[idx] := stringreplace(lbObjects.Items[idx], fieldValue, memoDescription.Text,[rfReplaceAll])
          else
            lbObjects.Items[idx] := stringreplace(lbObjects.Items[idx], fieldValue, memoShortDescription.Text,[rfReplaceAll])
        end;

        if AnsiContainsStr(lbObjects.Items[idx], '"focus":"') then
        begin
          fieldValue := ExtractField(lbObjects.Items[idx], '"focus":"');
          lbObjects.Items[idx] := stringreplace(lbObjects.Items[idx], fieldValue, edFocus.Text,[rfReplaceAll])
        end;
        if AnsiContainsStr(lbObjects.Items[idx], '"name":"') then
        begin
          fieldValue := ExtractField(lbObjects.Items[idx], '"name":"');
          lbObjects.Items[idx] := stringreplace(lbObjects.Items[idx], fieldValue, edName.Text,[rfReplaceAll])
        end;
      end;
      lbObjects.Items.SaveToFile(FHomeCases + FPathCase +  FuuidCase + '-CASE.json');
    end;
  end
  else
    ShowMessage('No case is selected');

end;

procedure TformMain.btnPRClick(Sender: TObject);
begin
// debub I put the fix value to 43E9FE90-F5DC-47C4-95F6-3C5BD6DFAA77
//  formIdentity.ShowWithParamater(uuidCase);
  if lbObjects.Items.Count = 0 then
    ShowMessage('Select a case or create a new case')
  else
    formProvenanceRecord.ShowWithParamater(FHomeCases + FPathCase, FuuidCase);

end;

procedure TformMain.btnRoleClick(Sender: TObject);
begin
  if lbObjects.Items.Count = 0 then
    ShowMessage('Select a case or create a new case')
  else
    formRole.ShowWithParamater(FHomeCases + FPathCase, FuuidCase);

end;

procedure TformMain.btnSelectCaseClick(Sender: TObject);
var
  line, caseID: String;
begin
  if cbCases.ItemIndex = -1 then
    ShowMessage('Select a case from the combo box on the right')
  else
  begin
    lbObjects.Items.Clear;
    line := cbCases.Items[cbCases.ItemIndex];
    caseID := Copy(line, Pos('@', line) + 1, Length(line));
    SetuuidCase(caseID);
    lbObjects.Items.LoadFromFile(caseID + '-CASE.json');
  end;
end;

procedure TformMain.btnToolClick(Sender: TObject);
begin
// debub I put the fix value to 43E9FE90-F5DC-47C4-95F6-3C5BD6DFAA77
//  formIdentity.ShowWithParamater(uuidCase);
  if lbObjects.Items.Count = 0 then
    ShowMessage('Select a case or create a new case')
  else
    formTool.ShowWithParamater(FhomeCases + FpathCase, FuuidCase);
end;

procedure TformMain.btnTraceClick(Sender: TObject);
begin
  if cbTrace.ItemIndex = - 1 then
  begin
    ShowMessage('Select a kind of trace');
    Exit;
  end;

  if cbCases.ItemIndex = - 1 then
    ShowMessage('Select a case or create a new case')
  else
  begin
    case cbTrace.ItemIndex of
      0: formTraceApplication.ShowWithParamater(FhomeCases + FpathCase, FuuidCase);
      1: formTraceApplicationAccount.ShowWithParamater(FhomeCases + FpathCase, FuuidCase);
      2: formTraceChat.ShowWithParamater(FhomeCases + FpathCase, FuuidCase);
      3: formTraceComputer.ShowWithParamater(FhomeCases + FpathCase, FuuidCase);
      4: formTraceDisk.ShowWithParamater(FhomeCases + FpathCase, FuuidCase);
      5: formTraceEmail.ShowWithParamater(FhomeCases + FpathCase, FuuidCase);
      6: formTraceEmailAccount.ShowWithParamater(FhomeCases + FpathCase, FuuidCase);
      7: formTraceFile.ShowWithParamater(FhomeCases + FpathCase, FuuidCase);
      8: formTraceDiskPartition.ShowWithParamater(FhomeCases + FpathCase, FuuidCase);
      9: formTraceMobileAccount.ShowWithParamater(FhomeCases + FpathCase, FuuidCase);
      10: formTraceMobile.ShowWithParamater(FhomeCases + FpathCase, FuuidCase);
      11: formTracePhoneAccount.ShowWithParamater(FhomeCases + FpathCase, FuuidCase);
      12: formTracePhoneCall.ShowWithParamater(FhomeCases + FpathCase, FuuidCase);
      13: formTraceSIM.ShowWithParamater(FhomeCases + FpathCase, FuuidCase);
      14: formTraceSMS.ShowWithParamater(FhomeCases + FpathCase, FuuidCase);
      15: formTraceUrlHistory.ShowWithParamater(FhomeCases + FpathCase, FuuidCase);
      else
        ShowMessage('Form has not implemented yet');
    end
  end;
end;

procedure TformMain.btnWarrantClick(Sender: TObject);
begin
// debub I put the fix value to 43E9FE90-F5DC-47C4-95F6-3C5BD6DFAA77
//  formIdentity.ShowWithParamater(uuidCase);
  if lbObjects.Items.Count = 0 then
    ShowMessage('Select a case or create a new case')
  else
    formWarrant.ShowWithParamater(FhomeCases + FpathCase, FuuidCase);

end;

procedure TformMain.btnWriteTtreeClick(Sender: TObject);
var
  idx, idy:Integer;
  node:TTreeViewItem;

begin
end;

procedure TformMain.btnNewCaseClick(Sender: TObject);
var
  Uid: TGuid;
  Result: HResult;
  uuidGenerated, sCase, crlf, recSep, caseName, caseID, caseFocus: String;
  operation: Integer;
begin

  operation := formNewCase.ShowWithParamater();
  if operation > 0 then
  begin
    edName.Text := '';
    edFocus.Text := '';
    memoShortDescription.Lines.Clear;
    memoDescription.Lines.Clear;
    uuidGenerated := generateUUID(Sender);
    uuidGenerated := Copy(uuidGenerated, 2, Length(uuidGenerated) - 2);
    SetuuidCase(uuidGenerated); // set the private field FuuidCase
    SetPathCase(uuidGenerated); // set the property FCasePath, the path of the new Case
    ForceDirectories(FHomeCases + FPathCase); // create the folder of the new Case
    edName.Text := formNewCase.edName.Text;
    edFocus.Text := formNewCase.edFocus.Text;
    memoShortDescription.Lines := formNewCase.memoShortDescription.Lines;
    memoDescription.Lines := formNewCase.memoDescription.Lines;
  // insert of the main data on the case
    lbObjects.Items.Clear;
    if (Trim(edName.Text) = '') or (memoShortDescription.Lines.Count = 0)  then
    else
    begin
      addRootCase(Sender);
      addRootChildren(Sender, uuidGenerated);
    //sCase := lbObjects.Items.Text;
      recSep := #30 + #30;
      crlf := #13 + #10;
    //sCase := stringreplace(sCase, recSep, crlf,[rfReplaceAll]);
      lbObjects.Items.SaveToFile(FHomeCases + FPathCase +  FuuidCase + '-CASE.json');
      caseName := ExtractField(lbObjects.Items.Text, '"uco-core:__caseNationalNumber":"');
      //caseID := ExtractField(lbObjects.Items.Text, '"-investigation');
      //caseID := uuidGenerated;
      caseFocus := ExtractField(lbObjects.Items.Text, '"uco-core:focus":"');
      cbCases.Items.Add(caseFocus + ' ' + caseName + '@' + uuidGenerated);
      // select the new case just added
      cbCases.ItemIndex := cbCases.Items.IndexOf(caseFocus + ' ' + caseName + '@' + uuidGenerated)
    end;
  end;

end;

procedure TformMain.btnInvestigativeActionClick(Sender: TObject);
begin
//  if lbObjects.Items.Count = 0 then
//    ShowMessage('Select a case or create a new case')
//  else
//    if cbActions.ItemIndex > - 1 then
  formInvestigativeAction.ShowWithParamater(FHomeCases + FPathCase, FuuidCase);
end;

procedure TformMain.btnIdentityClick(Sender: TObject);
begin
  if lbObjects.Items.Count = 0 then
    ShowMessage('Select a case or create a new case')
  else
    formIdentity.ShowWithParamater(FHomeCases + FPathCase, FuuidCase);
end;

procedure TformMain.btnRelationshipClick(Sender: TObject);
begin
  if lbObjects.Items.Count = 0 then
    ShowMessage('Select a case or create a new case')
  else
    formRelationship.ShowWithParamater(FhomeCases + FpathCase, FuuidCase);
end;

procedure TformMain.btnExtendCaseClick(Sender: TObject);
begin
//  ShowMessage('Not implemented yet');
end;

procedure TformMain.btnGenerateCaseJsonClick(Sender: TObject);
var
  idx: Integer;
  crlf, recSep, space4, space8, line, subStr: String;
begin
  if lbObjects.Items.Count = 0 then
    ShowMessage('No CASE selected')
  else
  begin
      //1. read lbObjects until the line contains '"object":['
      //2. the above line must not write in the final CASE/JSON file
      //3. write the line "object":[
      //4. open all files of the type FuuidCase-X con x=identity, location, trace-xxx ecc.
      //5. extract the value of the propertyName @id and write the value '"' + value + '"',
      //   except for the last id: no comma and add the string ']},'
      //   and save the line in the temporary array aObjects
      //6. add each item of the array aObjects
      //7. add the last two character ]}
      btnGenerateCaseJson.Cursor := crHourGlass;
      memoJSON.Lines.Clear;


      crlf := #13 + #10;
      recSep := #30 + #30;
      space4 := '    ';
      space8 := space4 + space4;
      subStr := 'investigation-';

      for idx:=0 to lbObjects.Items.Count - 1 do
      begin
        line := lbObjects.Items[idx];
        line := stringreplace(line, recSep, crlf,[rfReplaceAll]);
        line := stringreplace(line, space8, '',[rfReplaceAll]);
        line := stringreplace(line, space4, '',[rfReplaceAll]);
        memoJSON.Lines.Add(line)
      end;
      WriteObjectCASE;
      memoJSON.Lines.Add(']}');
      btnGenerateCaseJson.Cursor := crDefault;
  end;
end;

procedure TformMain.btnGenerateJSONClick(Sender: TObject);
var
  selectedFile: string;
  fileJSON: TextFile;
  line, subLine, crlf, recSep, space4, space8: String;
begin
  crlf := #13 + #10;
  recSep:= #30 + #30;
  //OpenDialog1.InitialDir := 'C:\';
  OpenDialog1.Filter := 'JSON files (*.json)|*.json';
  if OpenDialog1.Execute then
  begin
    selectedFile := OpenDialog1.FileName;
    AssignFile(fileJSON,selectedFile);
    Reset(fileJSON);
    memoJSON.Lines.Clear;
    while not Eof(fileJSON) do
    begin
      ReadLn(fileJSON, line);
      line := Trim(line);
      line := stringreplace(line, recSep, crlf,[rfReplaceAll]);
      memoJSON.Lines.Add(line);
    end;
  end;

end;

procedure TformMain.btnTimelineClick(Sender: TObject);
begin
  if cbCases.ItemIndex > -1 then
    formOverview.ShowWithParamater(FHomeCases + FPathCase, FuuidCase, edFocus.Text)
  else
    ShowMessage('No case is selected!');
end;

procedure TformMain.btnSaveToJSONClick(Sender: TObject);
begin
  saveDialog.Title := 'Save CASE/JSON in a JSON file';
  saveDialog.InitialDir := FHomeCases;
  saveDialog.DefaultExt := 'json';
  saveDialog.FileName :=  'CaseTest.json';
  if SaveDialog.Execute then
    memoJSON.Lines.SaveToFile(saveDialog.FileName);
end;


procedure TformMain.cbCasesChange(Sender: TObject);
var
  line, caseID: String;
  caseListString: TStringList;
begin
  if cbCases.ItemIndex = -1 then
    ShowMessage('Select a case from the combo box on the right')
  else
  begin
    //lbObjects.Items.Clear;
    edName.Text := '';
    edFocus.Text := '';
    memoShortDescription.Lines.Clear;
    memoDescription.Lines.Clear;
    memoJSON.Lines.Clear;
    line := cbCases.Items[cbCases.ItemIndex];
    caseID := Copy(line, Pos('@', line) + 1, Length(line));
    SetuuidCase(caseID);
    SetPathCase(caseID);
    //lbObjects.Items.Clear;
    caseListString := TStringList.Create;
    caseListString.LoadFromFile(FHomeCases + FPathCase + caseID + '-CASE.json');
    line := caseListString.Text;
    edFocus.Text := ExtractField(line, '"uco-core:focus":"');
    edName.Text := ExtractField(line, '"uco-core:__caseNationalNumber":"');
    memoShortDescription.Lines.Add(ExtractField(line, '"uco-core:name":"'));
    memoDescription.Lines.Add(ExtractField(line, '"uco-core:description":"'));

    //lbObjects.Items.Add(caseListString.Text);
    lbObjects.Items.LoadFromFile(FHomeCases + FPathCase + caseID + '-CASE.json');
  end;

end;

procedure TformMain.ExtractAllFiles(path: String);
var
  caseName, caseID, caseFocus: String;
  idx, casePos: Integer;
  caseFile, caseFolders  : TStringList;
  searchResult: TSearchRec;
begin
  caseFile := TStringList.Create;
  caseFolders := TStringList.Create;

  if FindFirst(path + '*', faAnyFile, searchResult) = 0 then
    begin
      try
        repeat
          if (searchResult.Attr = faDirectory) then
          begin
            if (searchResult.Name <> '.') and (searchResult.Name <> '..') then
              caseFolders.Add(searchResult.Name); // add folder containing data of the case
          end;
        until FindNext(searchResult) <> 0;
      finally
        FindClose(searchResult);
      end;
    end;

  caseFolders.Sort;
  for idx:=0 to caseFolders.Count -1 do
  begin
    path :=  FHomeCases + caseFolders[idx] + PathDelim ;
    if FindFirst(path + '*-CASE.json', faArchive, searchResult) = 0 then
    try
      repeat
        if (searchResult.Attr and faDirectory) = 0 then
        begin
          caseFile.LoadFromFile(path + searchResult.Name);
          caseName := ExtractField(caseFile.Text, '"uco-core:__caseNationalNumber":"');
          caseID := ExtractField(caseFile.Text, '"bundle-');
          caseFocus := ExtractField(caseFile.Text, '"uco-core:focus":"');
          cbCases.Items.Add(caseFocus + ' ' + caseName + '@' + caseID);
        end
        else
          if (searchResult.Name <> '.') and (searchResult.Name <> '..') then
            ExtractAllFiles(searchResult.Name);  // recursive call!
      until FindNext(searchResult) <> 0;
    finally
      FindClose(searchResult);
    end;
  end;
end;

procedure TformMain.ExtractAllItemsFromTreeView(tvNode: TTreeViewItem);
var
  idx: Integer;
begin
  lbObjects.Items.Add(tvNode.Text);
  for idx:=0 to tvNode.ChildrenCount do
    //ExtractAllItemsFromTreeView(tvItem.Items[0]);
  begin
    ShowMessage(tvNode.Items[idx].Text);
    if tvNode.Items[idx].ChildrenCount > 0 then
      ExtractAllItemsFromTreeView(tvNode.Items[idx]);
  end;
end;

function TformMain.ExtractField(line, subLine: String): String;
var
  fieldValue: String;
  fieldStart, fieldEnd: Integer;
begin
  fieldStart := Pos(subLine, line); // search pos of subLine inside line
  fieldValue := Copy(line, fieldStart + Length(subLine), Length(line));
  fieldEnd   := Pos('"', fieldValue);
  Result := Copy(fieldValue, 1, fieldEnd - 1);
end;

procedure TformMain.FormCreate(Sender: TObject);
//var
//  firstTreeViewItem: TTreeViewItem;
begin
  {firstTreeViewItem := TTreeViewItem.Create(nil);
  firstTreeViewItem.Text := 'My Tree';
  tvObjects.InsertObject(0, firstTreeViewItem);
  }

end;

procedure TformMain.FormShow(Sender: TObject);

begin
  // set the property FCaseHome: the home of all Cases (default: HOME/E2Ecases/
  SetHomeCases('');
  // create folder for Cases if it does not exist
  if not TDirectory.Exists(FHomeCases) then
    //TDirectory.CreateDirectory('test');
    ForceDirectories(FHomeCases);

  ExtractAllFiles(FHomeCases);
  lbObjects.Visible := False;

end;

function TformMain.generateUUID(Sender: TObject): String;
var
  Uid: TGUID;
  Value: HResult;
  uuidBraced : String;
begin
    Value := CreateGUID(Uid);
    uuidBraced := GuidToString(Uid);
    Result := Copy(uuidBraced, 2, Length(uuidBraced) - 2);
end;

procedure TformMain.SetHomeCases(const Value: String);
begin
  if Trim(Value) = '' then
    if Trim(GetEnvironmentVariable('HOMEPATH')) = '' then
      FHomeCases := GetEnvironmentVariable('HOMEDRIVE') + GetHomePath + PathDelim + 'E2Ecases' + PathDelim
    else
      FHomeCases := GetEnvironmentVariable('HOMEDRIVE') + GetEnvironmentVariable('HOMEPATH') + PathDelim + 'E2Ecases' + PathDelim
  else
    FHomeCases := Value;
end;

procedure TformMain.SetPathCase(const Value: String);
begin
  FPathCase := Value + PathDelim;
end;

procedure TformMain.SetuuidCase(const Value: String);
begin
  FuuidCase := Value;
end;

procedure TformMain.WriteObjectCASE;
var
  caseName, objectID, line: String;
  recSep, crlf, space4, space8: String;
  resFiles, idx, idy, casePos: Integer;
  caseList, objectsList, IDList, listFiles: TStringList;
  searchResult: TSearchRec;
begin
  caseList := TStringList.Create;
  objectsList := TStringList.Create;
  IDList    := TStringList.Create;
  listFiles := TStringList.Create;
  listFiles.Sorted := True;   // files ordered by name

  resFiles := FindFirst(FHomeCases + FpathCase + FuuidCase + '*.json', faAnyfile, searchResult);

  if resFiles = 0 then
  try
    while resFiles = 0 do
    begin
      if (searchResult.Attr and faDirectory <> faDirectory) then
      begin
        // ignore the general data of the Case
        if (AnsiContainsStr(searchResult.Name, '-CASE')) then
          // skip to the read next file
        else
        begin
          listFiles.Add(FHomeCases + Fpathcase + searchResult.Name);
        end;
      end;
      resFiles := FindNext(searchResult);
    end;
  finally
    FindClose(searchResult)
  end;

  idy:= 0;
  for idy:=0 to listFiles.Count - 1 do
  begin
    caseList.LoadFromFile(listFiles[idy]);
    for idx:=0 to caseList.Count - 1 do
      begin
        line := Trim(caseList[idx]);
        // first or last line or root element
        if ((line = '') or (line = '{') or (line = '}') or  (line = ']') or (AnsiContainsStr(line, 'OBJECTS_')))  then
        else
        begin
          if Copy(line, Length(line), 1) = ',' then
            objectsList.Add(line)
          else
            objectsList.Add(line + ',');
          end;
        end;
  end;



//  if objectsList.Count > 0 then
//    memoJSON.Lines.Add(line + ']},')
//  else
//    memoJSON.Lines.Add(line + ']}');


  crlf := #13 + #10;
  recSep := #30 + #30;
  space4 := '    ';
  space8 := space4 + space4;
  for idx:=0 to objectsList.Count - 2 do
  begin
    line := Trim(objectsList[idx]);
    line := stringreplace(line, recSep, crlf,[rfReplaceAll]);
    line := stringreplace(line, space8, '',[rfReplaceAll]);
    line := stringreplace(line, space4, '',[rfReplaceAll]);
    memoJSON.Lines.Add(line);
  end;


  if objectsList.Count > 0 then
  begin
    line := Copy(objectsList[idx], 1 , Length(objectsList[idx]) - 1);
    line := stringreplace(line, recSep, crlf,[rfReplaceAll]);
    line := stringreplace(line, space8, '',[rfReplaceAll]);
    line := stringreplace(line, space4, '',[rfReplaceAll]);
    memoJSON.Lines.Add(line);
  end;
end;

end.
