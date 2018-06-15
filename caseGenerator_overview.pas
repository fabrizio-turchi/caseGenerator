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
    Label5: TLabel;
    edWho: TEdit;
    Label6: TLabel;
    edRole: TEdit;
    edWhere: TEdit;
    edWhen: TEdit;
    Button1: TButton;
    Label7: TLabel;
    Edit1: TEdit;
    Label8: TLabel;
    Edit2: TEdit;
    TreeView1: TTreeView;
    tvMobile: TTreeViewItem;
    tvSIM: TTreeViewItem;
    tvFiles: TTreeViewItem;
    Label9: TLabel;
    edObject: TEdit;
    TreeViewItem1: TTreeViewItem;
    TreeViewItem2: TTreeViewItem;
    TreeViewItem4: TTreeViewItem;
    TreeViewItem5: TTreeViewItem;
    btnAddChild: TButton;
    edItemName: TEdit;
    TreeViewItem3: TTreeViewItem;
    TreeViewItem6: TTreeViewItem;
    TreeViewItem7: TTreeViewItem;
    procedure btnAddChildClick(Sender: TObject);
    procedure tvActionsChange(Sender: TObject);
  private
    FuuidCase: string;
    FPathCase: String;
    FlistActions: TStringList;
    FlistIdentities: TStringList;
    FlistRelationships: TSTringList;
    FlistRoles: TStringList;
    FlistProvenanceRecords: TStringList;
    FlistWarrants: TSTringList;
    FlistFiles: TStringList;
    FlistSIMs: TStringList;
    FlistLocations: TStringList;
    FlistMobiles: TStringList;
    FlistTools: TStringList;
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
    property listRelationships: TSTringList read FlistRelationships write SetlistRelationships;
    function readObjectsFromFile(fileObject: String): TStringList;
    procedure addTreeViewItemActionToRoot(actionName: String);
    { Private declarations }
  public
    procedure ShowWithParamater(pathCase:String; uuidCase: String);
    { Public declarations }
  end;

var
  formOverview: TformOverview;

implementation

{$R *.fmx}

procedure TformOverview.addTreeViewItemActionToRoot(actionName: String);
var
  itemRoot, itemNode: TTreeViewItem;
begin
  itemRoot := tvActions.ItemByText('Digital Evidence timeline');
  itemNode := TTreeViewItem.Create(Self);
  itemNode.Text := actionName;
  itemNode.Parent := itemRoot;
end;

procedure TformOverview.btnAddChildClick(Sender: TObject);
var
  itemRoot: TTreeViewItem;
  itemContext, itemGeneric: TTreeViewItem;
begin
  itemContext := tvActions.Selected;
  itemGeneric := TTreeViewItem.Create(Self);
  itemGeneric.Text := edItemName.Text;
  itemGeneric.Parent := itemContext;
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

procedure TformOverview.SetlistActions(const Value: TStringList);
begin
  FlistActions := Value;
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

procedure TformOverview.SetlistMobiles(const Value: TStringList);
begin
  FlistMobiles := Value;
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
  6.  using 1.f read trace_XXX.JSON if 1.a=preservec otherwise read provenance_record.JSON
      for extracting uuid values and use them for reading descriptive info from trace_XXX.JSON
}
var
  sDescription, sDate, sTime:string;
  idx : Integer;
begin
  SetUuidCase(uuidCase);
  SetPathCase(pathCase);

  FListActions := TStringList.Create;  // create of the property containing the investigative actions

  SetListActions(readObjectsFromFile('-investigative_action.json'));

  { extract name, descsription and startTIme from all Investigative_Actions}
  for idx:=0 to FListActions.Count - 1 do
  begin
    sDescription := ExtractField(FListActions[idx], '"description":"');
    sDate   := Copy(ExtractField(FListActions[idx], '"startTime":"'), 1, 10);
    sTime := Copy(ExtractField(FListActions[idx], '"startTime":"'), 12, 8);

    addTreeViewItemActionToRoot(sDescription + ' (' + sDate + ' ' + sTime + ')');
  end;

  SetListIdentities(readObjectsFromFile('-identity.json'));
  SetListRoles(readObjectsFromFile('-role.json'));
  SetListRelationships(readObjectsFromFile('-relationship.json'));
  SetListLocations(readObjectsFromFile('-location.json'));
  SetListWarrants(readObjectsFromFile('-warrant.json'));
  SetListTools(readObjectsFromFile('-tool.json'));
  SetListMobiles(readObjectsFromFile('-traceMOBILE.json'));
  SetListSIMs(readObjectsFromFile('-traceSIM.json'));
  SetListFiles(readObjectsFromFile('-traceFILE.json'));


  formOverview.ShowModal;
end;

procedure TformOverview.tvActionsChange(Sender: TObject);
var
  itemGeneric: TTreeViewItem;
  idx: Integer;
  line, IdPerformer, IdLocation, IdInstrument, description: String;
  IdResult, IdObject: TStringList;
begin
  itemGeneric := tvActions.Selected;
  idx := tvActions.Selected.GlobalIndex;
  if idx = 0 then  // root - Digital Evidence Timeline,
    ShowMessage('text: ' + itemGeneric.Text + 'index: ' + IntToStr(idx))
  else
  begin
    line := FListActions[idx - 1];
    IdPerformer := ExtractField(line, '"performer":"');
    IdLocation := ExtractField(line, '"location":"');
    IdInstrument := ExtractField(line, '"instrument":"');
    description := ExtractField(line, '"description":"');
    IdObject := ExtractArray(line, '"object":[');
    IdResult := ExtractArray(line, '"result":[');
  end;

end;

end.
