unit caseGenerator_overview;
{
  the form display the essential information related to the Evidence timeline. The starting
  point is the list of Investigative_Action when an item is selected all the correlated data
  are shown: Who, BRole, Where, When, What, Instrument, Object/Input, Result/Output.
  The Result are shown in a TreeView component where each node is broken down in Mobule device,
  SIM, File, Messages, Phone Account, etc.
}
interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.TreeView,
  FMX.Layouts, FMX.ScrollBox, FMX.Memo, FMX.Edit, FMX.Controls.Presentation,
  FMX.StdCtrls, System.StrUtils, caseGenerator_util;

type
  TformOverview = class(TForm)
    tvActions: TTreeView;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lblResult: TLabel;
    edWho: TEdit;
    Label6: TLabel;
    edRole: TEdit;
    edWhere: TEdit;
    edWhen: TEdit;
    Label7: TLabel;
    Label8: TLabel;
    edInstrument: TEdit;
    tvTraces: TTreeView;
    Label9: TLabel;
    edObject: TEdit;
    memoWhat: TMemo;
    procedure btnAddChildClick(Sender: TObject);
    procedure tvActionsChange(Sender: TObject);
  private
    FuuidCase: string;
    FPathCase: String;
    FlistActions: TStringList;
    FlistIdentities: TStringList;
    FlistRelationships: TSTringList;
    FlistRoles: TStringList;
    FlistWarrants: TSTringList;
    FlistFiles: TStringList;
    FlistSIMs: TStringList;
    FlistProvenanceRecords: TStringList;
    FlistLocations: TStringList;
    FlistMobiles: TStringList;
    FlistTools: TStringList;
    FaMonth: TStringList;
    FlistEmailAccounts: TStringList;
    FlistDiskPartitions: TStringList;
    FlistMessages: TStringList;
    FlistPhoneAccounts: TStringList;
    procedure SetPathCase(const Value: String);
    procedure SetuuidCase(const Value: string);
    procedure SetlistActions(const Value: TStringList);
    procedure SetlistIdentities(const Value: TStringList);
    procedure SetlistFiles(const Value: TStringList);
    procedure SetlistLocations(const Value: TStringList);
    procedure SetlistMobiles(const Value: TStringList);
    procedure SetlistProvenanceRecords(const Value: TStringList);
    procedure SetlistRelationships(const Value: TSTringList);
    procedure SetlistRoles(const Value: TStringList);
    procedure SetlistSIMs(const Value: TStringList);
    procedure SetlistTools(const Value: TStringList);
    procedure SetlistWarrants(const Value: TSTringList);
    procedure SetaMonth(const Value: TStringList);
    procedure SetlistDiskPartitions(const Value: TStringList);
    procedure SetlistEmailAccounts(const Value: TStringList);
    procedure SetlistMessages(const Value: TStringList);
    procedure SetlistPhoneAccounts(const Value: TStringList);
    property UuidCase: string read FuuidCase write SetuuidCase;
    property PathCase: String read FPathCase write SetPathCase;
    property listActions: TStringList read FlistActions write SetlistActions;
    property listIdentities: TStringList read FlistIdentities write SetlistIdentities;
    property listRoles: TStringList read FlistRoles write SetlistRoles;
    property listLocations: TStringList read FlistLocations write SetlistLocations;
    property listWarrants: TSTringList read FlistWarrants write SetlistWarrants;
    property listMobiles: TStringList read FlistMobiles write SetlistMobiles;
    property listSIMs: TStringList read FlistSIMs write SetlistSIMs;
    property listFiles: TStringList read FlistFiles write SetlistFiles;
    property listProvenanceRecords: TStringList read FlistProvenanceRecords write SetlistProvenanceRecords;
    property listTools: TStringList read FlistTools write SetlistTools;
    property listDiskPartitions: TStringList read FlistDiskPartitions write SetlistDiskPartitions;
    property listEmailAccounts: TStringList read FlistEmailAccounts write SetlistEmailAccounts;
    property listPhoneAccounts: TStringList read FlistPhoneAccounts write SetlistPhoneAccounts;
    property listMessages: TStringList read FlistMessages write SetlistMessages;
    property listRelationships: TSTringList read FlistRelationships write SetlistRelationships;
    function readObjectsFromFile(fileObject: String): TStringList;
    function addTreeViewItemToRoot(childText: String; tvComponent: TTreeView; rootText: String; itemParent:TTreeViewItem) : TTreeViewItem;
    procedure addTreeViewRoot(rootText: String; tvComponent: TTreeView);
    property aMonth : TStringList read FaMonth write SetaMonth;
    procedure SetTraces();
    { Private declarations }
  public
    procedure ShowWithParamater(pathCase:String; uuidCase: String);
    { Public declarations }
  end;

var
  formOverview: TformOverview;

implementation

{$R *.fmx}

function TformOverview.addTreeViewItemToRoot(childText: String; tvComponent: TTreeView; rootText:String; itemParent:TTreeViewItem): TTreeViewItem;
var
  itemRoot, itemNode: TTreeViewItem;
begin
  if itemParent = nil then
    itemRoot := tvComponent.ItemByText(rootText)
  else
    itemRoot := itemParent;

  itemNode := TTreeViewItem.Create(Self);
  itemNode.Text := childText;
  itemNode.Parent := itemRoot;
  Result := itemNode;
end;

procedure TformOverview.addTreeViewRoot(rootText: String;
  tvComponent: TTreeView);
var
  itemNode: TTreeViewItem;
begin
  itemNode := TTreeViewItem.Create(Self);
  itemNode.Text := rootText;
  itemNode.Parent := tvComponent;
end;

procedure TformOverview.btnAddChildClick(Sender: TObject);
//var
//  itemRoot: TTreeViewItem;
//  itemContext, itemGeneric: TTreeViewItem;
begin
//  itemContext := tvActions.Selected;
//  itemGeneric := TTreeViewItem.Create(Self);
//  itemGeneric.Text := edItemName.Text;
//  itemGeneric.Parent := itemContext;
//itemRoot := tvObjects.ItemByIndex(0);

end;

function TformOverview.readObjectsFromFile(fileObject: String): TStringList;
var
  fileJSON: TextFile;
  line, subLine, dir:string;
  listObjects: TStringList;
begin
  listObjects := TStringList.Create;
  //dir := GetCurrentDir;
  // read file JSON uuidCase-investigative_action.json
  if FileExists(FPathCase + FUuidCase + fileObject) then
  begin
    AssignFile(fileJSON,  FPathCase + FUuidCase + fileObject);
    Reset(fileJSON);

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

        listObjects.Add(line);
      end;
    end;
    CloseFile(fileJSON);
  end;

  Result := listObjects;
end;

procedure TformOverview.SetaMonth(const Value: TStringList);
begin
  FaMonth := Value;
end;

procedure TformOverview.SetlistActions(const Value: TStringList);
begin
  FlistActions := Value;
end;

procedure TformOverview.SetlistDiskPartitions(const Value: TStringList);
begin
  FlistDiskPartitions := Value;
end;

procedure TformOverview.SetlistEmailAccounts(const Value: TStringList);
begin
  FlistEmailAccounts := Value;
end;

procedure TformOverview.SetlistFiles(const Value: TStringList);
begin
  FlistFiles := Value;
end;

procedure TformOverview.SetlistIdentities(const Value: TStringList);
begin
  FlistIdentities := Value;
end;

procedure TformOverview.SetlistLocations(const Value: TStringList);
begin
  FlistLocations := Value;
end;

procedure TformOverview.SetlistMessages(const Value: TStringList);
begin
  FlistMessages := Value;
end;

procedure TformOverview.SetlistMobiles(const Value: TStringList);
begin
  FlistMobiles := Value;
end;

procedure TformOverview.SetlistPhoneAccounts(const Value: TStringList);
begin
  FlistPhoneAccounts := Value;
end;

procedure TformOverview.SetlistProvenanceRecords(const Value: TStringList);
begin
  FlistProvenanceRecords := Value;
end;

procedure TformOverview.SetlistRelationships(const Value: TSTringList);
begin
  FlistRelationships := Value;
end;

procedure TformOverview.SetlistRoles(const Value: TStringList);
begin
  FlistRoles := Value;
end;

procedure TformOverview.SetlistSIMs(const Value: TStringList);
begin
  FlistSIMs := Value;
end;

procedure TformOverview.SetlistTools(const Value: TStringList);
begin
  FlistTools := Value;
end;

procedure TformOverview.SetlistWarrants(const Value: TSTringList);
begin
  FlistWarrants := Value;
end;

procedure TformOverview.SetPathCase(const Value: String);
begin
  FPathCase := Value;
end;

procedure TformOverview.SetTraces;
var
  itemText, model, manufacturer, msisdn: String;
  simForm, carrier: String;
  fileName, size: String;
  itemNode: TTreeViewItem;
  idx, idy : Integer;
begin
  lblResult.Text := 'Traces';
  tvTraces.Clear;
  edWho.Text := '';
  edRole.Text := '';
  edWhere.Text := '';
  edWhen.Text := '';
  memoWhat.Lines.Clear;
  edInstrument.Text := '';
  edObject.Text := '';
  addTreeViewRoot('Traces', tvTraces);

  if FlistMobiles.Count > 0 then
  begin
    itemText := 'Mobile devices (' + IntToStr(FlistMobiles.Count) + ')';
    itemNode := addTreeViewItemtoRoot(itemText, tvTraces, 'Traces', nil);
    for idx := 0 to FlistMobiles.Count - 1 do
    begin
      model := ExtractField(FlistMobiles[idx], '"model":"');
      manufacturer := ExtractField(FlistMobiles[idx], '"manufacturer":"');
      msisdn := ExtractField(FlistMobiles[idx], '"MSISDN":"');
      addTreeViewItemtoRoot(model+ ' ' + manufacturer + ' (' + msisdn + ')', tvTraces, itemText, itemNode);
    end;
  end;

  if FlistSIMs.Count > 0 then
  begin
    itemText := 'SIMs (' + IntToStr(FlistSIMs.Count) + ')';
    itemNode := addTreeViewItemtoRoot(itemText, tvTraces, 'Traces', nil);
    for idx := 0 to FlistSIMs.Count - 1 do
    begin
      simForm := ExtractField(FlistSIMs[idx], '"SIMForm":"');
      carrier := ExtractField(FlistSIMs[idx], '"Carrier":"');
      addTreeViewItemtoRoot(simForm + ' ' + carrier, tvTraces, itemText, itemNode);
    end;
  end;

  if FlistFiles.Count > 0 then
  begin
    itemText := 'FILEs (' + IntToStr(FlistFiles.Count) + ')';
    itemNode := addTreeViewItemtoRoot(itemText, tvTraces, 'Traces', nil);
    for idx := 0 to FlistFiles.Count - 1 do
    begin
      fileName := ExtractField(FlistFiles[idx], '"fileName":"');
      size := ExtractField(FlistFiles[idx], '"sizeInBytes":"');
      addTreeViewItemtoRoot(fileName + ' (' + size + ')', tvTraces, itemText, itemNode);
    end;
  end;

  if FlistDiskPartitions.Count > 0 then
  begin
    itemText := 'DISK PARTITIONs (' + IntToStr(FlistDiskPartitions.Count) + ')';
    itemNode := addTreeViewItemtoRoot(itemText, tvTraces, 'Traces', nil);
    for idx := 0 to FlistDiskPartitions.Count - 1 do
    begin
      fileName := ExtractField(FlistDiskPartitions[idx], '"diskPartitionType":"');
      size := ExtractField(FlistDiskPartitions[idx], '"partitionLength":"');
      addTreeViewItemtoRoot(fileName + ' (' + size + ')', tvTraces, itemText, itemNode);
    end;
  end;

  if FlistEmailAccounts.Count > 0 then
  begin
    itemText := 'EMAIL ACCOUNTs (' + IntToStr(FlistEmailAccounts.Count) + ')';
    itemNode := addTreeViewItemtoRoot(itemText, tvTraces, 'Traces', nil);
    for idx := 0 to FlistEmailAccounts.Count - 1 do
    begin
      fileName := ExtractField(FlistEmailAccounts[idx], '"emailAddress":"');
      addTreeViewItemtoRoot(fileName, tvTraces, itemText, itemNode);
    end;
  end;

  if FlistMessages.Count > 0 then
  begin
    itemText := 'SMSs (' + IntToStr(FlistMessages.Count) + ')';
    itemNode := addTreeViewItemtoRoot(itemText, tvTraces, 'Traces', nil);
    for idx := 0 to FlistMessages.Count - 1 do
    begin
      fileName := ExtractField(FlistMessages[idx], '"application":"');
      size := ExtractField(FlistMessages[idx], '"messageText":"');
      addTreeViewItemtoRoot(fileName + ' (' + size + ')', tvTraces, itemText, itemNode);
    end;
  end;

  if FlistPhoneAccounts.Count > 0 then
  begin
    itemText := 'PHONE ACCOUNTs (' + IntToStr(FlistPhoneAccounts.Count) + ')';
    itemNode := addTreeViewItemtoRoot(itemText, tvTraces, 'Traces', nil);
    for idx := 0 to FlistPhoneAccounts.Count - 1 do
    begin
      fileName := ExtractField(FlistPhoneAccounts[idx], '"phoneNumber":"');
      size := ExtractField(FlistPhoneAccounts[idx], '"accountIssuer":"');
      addTreeViewItemtoRoot(fileName + ' (' + size + ')', tvTraces, itemText, itemNode);
    end;
  end;

  tvTraces.ExpandAll;
end;

procedure TformOverview.SetuuidCase(const Value: string);
begin
  FuuidCase := Value;
end;

procedure TformOverview.ShowWithParamater(pathCase, uuidCase: String);
{
  1. read investigative_action.JSON for extracting the follwoing data
    a.  name
    b.  performer
    c.  location
    d.  instrument
    e.  description
    f.  object
    g.  result
  2.  using 1.b read identity.JSON and role.JSON for setting up the field [Who] and [Role]
  3.  using 1.c read location.JSON for setting up the field [Where]
  4.  using 1.d read warrant.JSON, if 1.a=preserved or transferred otherwise read tool.JSON
      for setting up the field [Instrument]
  5.  use 1.e for setting up the field [What]
  6.  using 1.f read trace_XXX.JSON if 1.a=preserved otherwise read provenance_record.JSON
      for extracting uuid values and use them for reading descriptive info from trace_XXX.JSON
}
var
  sDescription, sDate, sTime :string;
  idx : Integer;
  aMonths: TStringList;
begin
  SetUuidCase(uuidCase);
  SetPathCase(pathCase);

  aMonths := TStringList.Create;
  aMonths.Add('Jan');
  aMonths.Add('Feb');
  aMonths.Add('Mar');
  aMonths.Add('Apr');
  aMonths.Add('May');
  aMonths.Add('Jun');
  aMonths.Add('Jul');
  aMonths.Add('Aug');
  aMonths.Add('Sep');
  aMonths.Add('Oct');
  aMonths.Add('Nov');
  aMonths.Add('Dec');

  //FaMonth := TStringList.Create;
  SetaMonth(aMonths);

  tvActions.Clear;
  addTreeViewRoot('Digital Evidence Timeline', tvActions);

  FListActions := TStringList.Create;  // create of the property containing the investigative actions

  SetListActions(readObjectsFromFile('-investigative_action.json'));

  { extract name, descsription and startTIme from all Investigative_Actions}
  for idx:=0 to FListActions.Count - 1 do
  begin
    sDescription := ExtractField(FListActions[idx], '"description":"');
    sDate   := Copy(ExtractField(FListActions[idx], '"startTime":"'), 1, 10);
    sTime := Copy(ExtractField(FListActions[idx], '"startTime":"'), 12, 8);

    addTreeViewItemToRoot(sDescription + ' (' + sDate + ' ' + sTime + ')', tvActions, 'Digital Evidence Timeline', nil);
  end;

  SetListIdentities(readObjectsFromFile('-identity.json'));
  SetListRoles(readObjectsFromFile('-role.json'));
  SetListRelationships(readObjectsFromFile('-relationship.json'));
  SetListLocations(readObjectsFromFile('-location.json'));
  SetListWarrants(readObjectsFromFile('-warrant.json'));
  SetListTools(readObjectsFromFile('-tool.json'));
  SetListProvenanceRecords(readObjectsFromFile('-provenance_record.json'));
  SetListMobiles(readObjectsFromFile('-traceMOBILE.json'));
  SetListSIMs(readObjectsFromFile('-traceSIM.json'));
  SetListFiles(readObjectsFromFile('-traceFILE.json'));
  SetListDiskPartitions(readObjectsFromFile('-traceDISK_PARTITION.json'));
  SetListEmailAccounts(readObjectsFromFile('-traceEMAIL_ACCOUNT.json'));
  SetListMessages(readObjectsFromFile('-traceMESSAGE.json'));
  SetListPhoneAccounts(readObjectsFromFile('-tracePHONE_ACCOUNT.json'));

  //  The Result/Object TreeView component contains all thr Traces of the Case, when a
  //  specific Investigative_Action is selectes it only contains the Result/Output of the
  //  related Provenance_Record generated from that Investigative Action (Search and seizure,
  //  forensics acquistion, forensic extraction, etc.

  setTraces();
  formOverview.ShowModal;
end;

procedure TformOverview.tvActionsChange(Sender: TObject);
var
  //itemGeneric: TTreeViewItem;
  idx, idy, idk, idw, idn, nMonth: Integer;
  line, IdPerformer, IdLocation, IdInstrument, description, itemText: String;
  name, IdRole, IdIdentity, sDateTime, startTime, endTime: String;
  model, manufacturer, msisdn: String;
  simForm, carrier, fileName, size: String;
  IdResults, itemsObjectPR, IdObject: TStringList;
  itemsMobile, itemsSIM, itemsFile, itemsDiskPartition: TStringList;
  itemsMessage, itemsEmailAccount, itemsPhoneAccount: TStringList;
  lObjectFound: Boolean;
  itemNode: TTreeViewItem;
begin
  //itemGeneric := TTreeViewItem.Create(Self);
  //itemGeneric := tvActions.Selected;
  idx := tvActions.Selected.GlobalIndex;
  //showmessage('global index=' + IntToStr(idx));
  if idx = 0 then  // root - Digital Evidence Timeline,
    SetTraces()
  else
  begin
    edWho.Text := '';
    edRole.Text := '';
    edWhere.Text := '';
    edWhen.Text := '';
    memoWhat.Lines.Clear;
    edInstrument.Text := '';
    edObject.Text := '';
    tvTraces.Clear;
    lblResult.Text := 'Result/Output';
    line := FListActions[idx - 1];
    IdPerformer := ExtractField(line, '"performer":"');
    name := ExtractField(line, '"name":"');
    IdLocation := ExtractField(line, '"location":"');
    IdInstrument := ExtractField(line, '"instrument":"');
    startTime := ExtractField(line, '"startTime":"');
    endTime := ExtractField(line, '"endTime":"');
    description := ExtractField(line, '"description":"');
    IdObject := ExtractArray(line, '"object":[');
    IdResults := ExtractArray(line, '"result":[');
    // extract IdRole from Relationship
    for idx := 0 to FlistRelationships.Count - 1 do
    begin
      if AnsiContainsStr(FlistRelationships[idx], IdPerformer) then
      begin
        IdIdentity := ExtractField(FlistRelationships[idx], '"source":"');
        IdRole   := ExtractField(FlistRelationships[idx], '"target":"');
        break
      end;
    end;

    for idx := 0 to FlistIdentities.Count - 1 do
    begin
      if AnsiContainsStr(FlistIdentities[idx], IdIdentity) then
      begin
        edWho.Text := ExtractField(FlistIdentities[idx], '"givenName":"') + ' ' +
                      ExtractField(FlistIdentities[idx], '"familyName":"');
        break
      end;
    end;

    for idx := 0 to FlistRoles.Count - 1 do
    begin
      if AnsiContainsStr(FlistRoles[idx], IdRole) then
      begin
        edRole.Text := ExtractField(FlistRoles[idx], '"name":"');
        break
      end;
    end;

    for idx := 0 to FlistLocations.Count - 1 do
    begin
      if AnsiContainsStr(FlistLocations[idx], IdLocation) then
      begin
        edWhere.Text := ExtractField(FlistLocations[idx], '"street":"') + ' ' +
                      ExtractField(FlistLocations[idx], '"postalCode":"') + ' ' +
                      ExtractField(FlistLocations[idx], '"locality":"') + ' ' +
                      ExtractField(FlistLocations[idx], '"region":"');
        break;
      end;
    end;

    sDateTime := Copy(startTime, 1, 10);
    nMonth := StrToInt(Copy(sDateTime, 6, 2));
    edWhen.Text :=  Copy(sDateTime, 1, 4) + ' ' + FaMonth[nMonth - 1] + ' ' +
                    Copy(sDateTime, 9, 2);

    memoWhat.Text:=  description;

    if (name = 'preserved') or (name='transferred') then
    begin
      for idx := 0 to FlistWarrants.Count - 1 do
      begin
        if AnsiContainsStr(FlistWarrants[idx], IdInstrument) then
        begin
          edInstrument.Text := ExtractField(FlistWarrants[idx], '"authorizationIdentifier":"');
          break;
        end;
      end;
    end
    else
    begin
      for idx := 0 to FlistTools.Count - 1 do
      begin
        if AnsiContainsStr(FlistTools[idx], IdInstrument) then
        begin
          edInstrument.Text := ExtractField(FlistTools[idx], '"name":"');
          break;
        end;
      end;
    end;

    for idx := 0 to FlistProvenanceRecords.Count - 1 do
    begin
      if AnsiContainsStr(FlistProvenanceRecords[idx], IdObject[0]) then
      begin
        edObject.Text := ExtractField(FlistProvenanceRecords[idx], '"description":"');
        break;
      end;
    end;
    {
    read items from IdResults,
    for each IdResults[idx] item find uuid in FlistProvenanceRecord,
    when it macthes extract uuid of "object"
    find uuid object in all traces and extract information to put in Mobiles, SIMs, FIles of
    tvTraces
  }
    addTreeViewRoot('Results', tvTraces);
    itemsMobile := TStringList.Create;
    itemsSIM := TStringList.Create;
    itemsFile := TStringList.Create;
    itemsEmailAccount := TStringList.Create;
    itemsMessage := TStringList.Create;
    itemsDiskPartition := TStringList.Create;
    itemsPhoneAccount := TStringList.Create;

    itemsObjectPR := TStringList.Create;   // objects included in the Provenance_Record



    for idx:= 0 to IdResults.Count - 1 do
    begin

      for idy:=0 to FlistProvenanceRecords.Count - 1 do
      begin
        if AnsiContainsStr(FlistProvenanceRecords[idy], IdResults[idx]) then
        begin
            itemsObjectPR := ExtractArray(FlistProvenanceRecords[idy], '"object":[');
            if itemsObjectPR.Count = 0 then
            begin
              ShowMessage('Error: Provenance Record without Objects!');
              Exit;
            end;

            for idn:=0 to  itemsObjectPR.Count - 1 do
            begin
              lObjectFound := False;
              for idk := 0 to FlistMobiles.Count - 1 do
              begin
                if AnsiContainsStr(FlistMobiles[idk], itemsObjectPR[idn]) then
                begin
                  itemsMobile.Add(ExtractField(FlistMobiles[idk], '"model":"') +
                    ' ' +  ExtractField(FlistMobiles[idk], '"manufacturer":"') +
                    ' (' + ExtractField(FlistMobiles[idk], '"MSISDN":"') + ')');
                    lObjectFound := True;
                    break;
                end;
              end;

              for idk := 0 to FlistSIMs.Count - 1 do
              begin
                if lObjectFound then
                  break;
                if AnsiContainsStr(FlistSIMs[idk], itemsObjectPR[idn]) then
                begin
                  itemsSIM.Add(ExtractField(FlistSIMs[idk], '"SIMForm":"') +
                    ' ' +  ExtractField(FlistSIMs[idk], '"Carrier":"'));
                    lObjectFound := True;
                    break;
                end;
              end;

              for idk := 0 to FlistFiles.Count - 1 do
              begin
                if lObjectFound then
                  break;
                if AnsiContainsStr(FlistFiles[idk], itemsObjectPR[idn]) then
                begin
                  itemsFile.Add(ExtractField(FlistFiles[idk], '"fileName":"') +
                    ' (' +  ExtractField(FlistFiles[idk], '"sizeInBytes":"') + ')');
                    lObjectFound := True;
                    break;
                end;
              end;

              for idk := 0 to FlistEmailAccounts.Count - 1 do
              begin
                if lObjectFound then
                  break;
                if AnsiContainsStr(FlistEmailAccounts[idk], itemsObjectPR[idn]) then
                begin
                  itemsEmailAccount.Add(ExtractField(FlistEmailAccounts[idk], '"emailAddress":"'));
                  lObjectFound := True;
                  break;
                end;
              end;

              for idk := 0 to FlistMessages.Count - 1 do
              begin
                if lObjectFound then
                  break;
                if AnsiContainsStr(FlistMessages[idk], itemsObjectPR[idn]) then
                begin
                  itemsMessage.Add(ExtractField(FlistMessages[idk], '"application":"') +
                  ' (' + ExtractField(FlistMessages[idx], '"messageText":"') + ')');
                  lObjectFound := True;
                  break;
                end;
              end;

              for idk := 0 to FlistPhoneAccounts.Count - 1 do
              begin
                if lObjectFound then
                  break;
                if AnsiContainsStr(FlistPhoneAccounts[idk], itemsObjectPR[idn]) then
                begin
                  itemsMessage.Add(ExtractField(FlistPhoneAccounts[idk], '"phoneNumber":"') +
                  ' (' + ExtractField(FlistPhoneAccounts[idx], '"accountIssuer":"') + ')');
                  lObjectFound := True;
                  break;
                end;
              end;

              for idk := 0 to FlistDiskPartitions.Count - 1 do
              begin
                if lObjectFound then
                  break;
                if AnsiContainsStr(FlistPhoneAccounts[idk], itemsObjectPR[idn]) then
                begin
                  itemsMessage.Add(ExtractField(FlistDiskPartitions[idk], '"diskPartitionType":"') +
                  ' (' + ExtractField(FlistDiskPartitions[idx], '"partitionLength":"') + ')');
                  lObjectFound := True;
                  break;
                end;
              end;
          end;
        end;
      end;
    end;

    if itemsMobile.Count > 0 then
    begin
      itemText := 'Mobile devices (' + IntToStr(itemsMobile.Count) + ')';
      itemNode := addTreeViewItemtoRoot(itemText, tvTraces, 'Results', nil);
      for idw := 0 to itemsMobile.Count - 1 do
        addTreeViewItemtoRoot(itemsMobile[idw], tvTraces, itemText, itemNode);
    end;

    if itemsSIM.Count > 0 then
    begin
      itemText := 'SIMs (' + IntToStr(itemsSIM.Count) + ')';
      itemNode := addTreeViewItemtoRoot(itemText, tvTraces, 'Results', nil);
      for idw := 0 to itemsSIM.Count - 1 do
        addTreeViewItemtoRoot(itemsSIM[idw], tvTraces, itemText, itemNode);
    end;

    if itemsFile.Count > 0 then
    begin
      itemText := 'FILEs (' + IntToStr(itemsFile.Count) + ')';
      itemNode := addTreeViewItemtoRoot(itemText, tvTraces, 'Results', nil);
      for idw := 0 to itemsFile.Count - 1 do
        addTreeViewItemtoRoot(itemsFile[idw], tvTraces, itemText, itemNode);
    end;

    if itemsEmailAccount.Count > 0 then
    begin
      itemText := 'EMAIL ACCOUNTs (' + IntToStr(itemsEmailAccount.Count) + ')';
      itemNode := addTreeViewItemtoRoot(itemText, tvTraces, 'Results', nil);
      for idw := 0 to itemsEmailAccount.Count - 1 do
        addTreeViewItemtoRoot(itemsEmailAccount[idw], tvTraces, itemText, itemNode);
    end;

    if itemsMessage.Count > 0 then
    begin
      itemText := 'SMSs (' + IntToStr(itemsMessage.Count) + ')';
      itemNode := addTreeViewItemtoRoot(itemText, tvTraces, 'Results', nil);
      for idw := 0 to itemsMessage.Count - 1 do
        addTreeViewItemtoRoot(itemsMessage[idw], tvTraces, itemText, itemNode);
    end;

    if itemsDiskPartition.Count > 0 then
    begin
      itemText := 'DISK PARTITIONs (' + IntToStr(itemsDiskPartition.Count) + ')';
      itemNode := addTreeViewItemtoRoot(itemText, tvTraces, 'Results', nil);
      for idw := 0 to itemsDiskPartition.Count - 1 do
        addTreeViewItemtoRoot(itemsDiskPartition[idw], tvTraces, itemText, itemNode);
    end;

    if itemsPhoneAccount.Count > 0 then
    begin
      itemText := 'PHONE ACCOUNTs (' + IntToStr(itemsPhoneAccount.Count) + ')';
      itemNode := addTreeViewItemtoRoot(itemText, tvTraces, 'Results', nil);
      for idw := 0 to itemsPhoneAccount.Count - 1 do
        addTreeViewItemtoRoot(itemsPhoneAccount[idw], tvTraces, itemText, itemNode);
    end;

    tvTraces.ExpandAll;
  end;


end;

end.
