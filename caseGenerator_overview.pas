unit caseGenerator_overview;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.TreeView,
  FMX.Layouts, FMX.ScrollBox, FMX.Memo, FMX.Edit, FMX.Controls.Presentation,
  FMX.StdCtrls;

type
  TformOverview = class(TForm)
    tvActions: TTreeView;
    tvSearchAndSeizure: TTreeViewItem;
    tvExtraction: TTreeViewItem;
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
    btnAddItem: TButton;
    btnAddChild: TButton;
    edItemName: TEdit;
    tvTransfer: TTreeViewItem;
    TreeViewItem3: TTreeViewItem;
    TreeViewItem6: TTreeViewItem;
    TreeViewItem7: TTreeViewItem;
    TreeViewItem8: TTreeViewItem;
    procedure btnAddItemClick(Sender: TObject);
    procedure btnAddChildClick(Sender: TObject);
    procedure tvActionsChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  formOverview: TformOverview;

implementation

{$R *.fmx}

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

procedure TformOverview.btnAddItemClick(Sender: TObject);
var
  itemRoot: TTreeViewItem;
  itemContext, itemGeneric: TTreeViewItem;
begin
  itemRoot := tvActions.ItemByText('Digital Evidence timeline');
  itemGeneric := TTreeViewItem.Create(Self);
  itemGeneric.Text := edItemName.Text;
  itemGeneric.Parent := itemRoot;
  //itemRoot := tvObjects.ItemByIndex(0);
  //itemContext := TTreeViewItem.Create(nil);
  //itemContext.Text := '"@context":{' + recSep;
  //itemContext.ParentItem :=itemRoot;
  //tvObjects.InsertObject(0, itemContext);
  //addChildren(itemContext, '"@vocab": "http://case.example.org/core#",' + recSep);
  //addChildren(itemContext, '"case": "http://case.example.org/core#",' + recSep);
  //addChildren(itemContext, '"rdf": "http://www.w3.org/1999/02/22-rdf-syntax-ns#",' + recSep);
  //addChildren(itemContext, '"rdfs": "http://www.w3.org/2000/01/rdf-schema#",' + recSep);
  //addChildren(itemContext, '"xsd": "http://www.w3.org/2001/XMLSchema#"},' + recSep);
end;

procedure TformOverview.tvActionsChange(Sender: TObject);
var
  itemGeneric: TTreeViewItem;
  idx: Integer;
begin
  itemGeneric := tvActions.Selected;
  idx := tvActions.Selected.GlobalIndex;
  ShowMessage('text: ' + itemGeneric.Text + 'index: ' + IntToStr(idx));

end;

end.
