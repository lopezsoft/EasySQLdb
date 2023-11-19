unit EDBStringsPropertyEditorDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, strutils,
  SynEdit, ButtonPanel, SynHighlighterSQL, ComCtrls, db, DBGrids, Menus,
  SrcEditorIntf, easyDB, clipbrd, StdCtrls, fpsqltree, fpsqlparser;

type

  { TDBStringsPropertyEditorDlg }

  TDBStringsPropertyEditorDlg = class(TForm)
    ButtonsPanel: TButtonPanel;
    CbxMetaData: TComboBox;
    SQLMeta: TEDBQuery;
    SQLQuery: TEDBQuery;
    MIPaste: TMenuItem;
    MetaDBGrid: TDBGrid;
    EdtObject: TEdit;
    ImageList: TImageList;
    Label1: TLabel;
    MIMeta: TMenuItem;
    MIMetaColumns: TMenuItem;
    MICheck: TMenuItem;
    MICreateConstant: TMenuItem;
    MICleanup: TMenuItem;
    PMSQL: TPopupMenu;
    PMMeta: TPopupMenu;
    ResultDBGrid: TDBGrid;
    SQLDataSource: TDatasource;
    OpenDialog: TOpenDialog;
    PageControl: TPageControl;
    SaveDialog: TSaveDialog;
    SQLDataSource1: TDatasource;
    SQLEditor: TSynEdit;
    SQLHighlighter: TSynSQLSyn;
    EditorTabSheet: TTabSheet;
    ResultTabSheet: TTabSheet;
    MetaTabSheet: TTabSheet;
    TBConst: TToolButton;
    ToolBar: TToolBar;
    OpenToolButton: TToolButton;
    SaveToolButton: TToolButton;
    DividerToolButton: TToolButton;
    ExecuteToolButton: TToolButton;
    TBCheck: TToolButton;
    procedure MetaDBGridDblClick(Sender: TObject);
    procedure ExecuteToolButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MICleanupClick(Sender: TObject);
    procedure MICreateConstantClick(Sender: TObject);
    procedure MIMetaColumnsClick(Sender: TObject);
    procedure MIPasteClick(Sender: TObject);
    procedure OpenToolButtonClick(Sender: TObject);
    procedure SaveToolButtonClick(Sender: TObject);
    procedure SQLEditorMouseDown(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: Integer);
    procedure TBCheckClick(Sender: TObject);
    procedure TBConstClick(Sender: TObject);
  private
    { private declarations }
    FMetaFromSynedit: Boolean;
    FConnection:TEDBConnection;
    FISSQLScript: Boolean;
    FTransaction:TEDBTransaction;
    FWordUnderCursor:string;
    function CheckConnection:boolean;
    procedure CheckSQLSyntax({%H-}SQL: TStrings);
    procedure CleanupDelphiCode;
    procedure CreateConstant;
    procedure ShowMetaData;
  public
    { public declarations }
    constructor Create(AOwner:TComponent);override;
  published
    property Connection: TEDBConnection   read FConnection  write FConnection;
    property Transaction:TEDBTransaction read FTransaction write FTransaction;
    Property IsSQLScript : Boolean Read FISSQLScript Write FIsSQLScript;
  end; 

implementation

{$R *.lfm}

resourcestring
  SResultTabCaption = 'Resultados';
  SSQLTabCaption    = 'Cídog SQL';
  SMetaTabCaption   = 'Metadatos';
  SMetaTables       = 'Tablas';
  SMetaColumns      = 'Columnas';
  SMetaProcedures   = 'Procedimientos';
  SMetaPleaseSpecifyATableInTheObjectField = 'Por favor especifique una tabla '
    +'en la fila del obejto.';
  SMetaSysTables    = 'SysTables';

  SLoadSQLCodeHint = 'Cargar código SQL ...';
  SSaveSQLCodeHint = 'Guardar cóidog SQL ...';
  SRunSQLCodeHint = 'Ejecutar código SQL';
  SQuickCheckOfSQLSyntaxHint = 'Comprobación rápida de la sintaxis de SQL';

  // SQL Parser results:
  // Note: sql parser is not quite exact, so indicate it's not completely sure
  SSQLOK            = 'Comprobación rápida de SQL OK';
  SQLSyntaxOK       = 'No se encontraron errores de sintaxis en la instrucción SQL.';
  SSQLError         = 'Probable error SQL';
  SSQLSyntaxError   = 'Error de sintaxis probable en la instrucción SQL:'+slineBreak+'%s';

{ TDBStringsPropertyEditorDlg }

//----------------------------------------------------------------//
constructor TDBStringsPropertyEditorDlg.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SourceEditorManagerIntf.GetEditorControlSettings(SQLEditor);
  SourceEditorManagerIntf.GetHighlighterSettings(SQLHighlighter);
  EditorTabSheet.Caption := SSQLTabCaption;
  ResultTabSheet.Caption := SResultTabCaption;
  MetaTabSheet.Caption := SMetaTabCaption;
  OpenToolButton.Hint := SLoadSQLCodeHint;
  SaveToolButton.Hint := SSaveSQLCodeHint;
  ExecuteToolButton.Hint := SRunSQLCodeHint;
  TBCheck.Hint := SQuickCheckOfSQLSyntaxHint;
  CbxMetaData.Items.Add(SMetaTables);
  CbxMetaData.Items.Add(SMetaSysTables);
  CbxMetaData.Items.Add(SMetaColumns);
  CbxMetaData.Items.Add(SMetaProcedures);
end;

//----------------------------------------------------------//
function TDBStringsPropertyEditorDlg.CheckConnection:boolean;
begin
  Result := (Assigned(FConnection)) and (Assigned(FTransaction))
             and(FConnection.Connected);
end;

//------------------------------------------------------------------------//
procedure TDBStringsPropertyEditorDlg.OpenToolButtonClick(Sender: TObject);
begin
  if(OpenDialog.Execute)then
    SQLEditor.Lines.LoadFromFile(OpenDialog.FileName);
end;

//---------------------------------------------------------------------------//
procedure TDBStringsPropertyEditorDlg.ExecuteToolButtonClick(Sender: TObject);
begin
  FMetaFromSynedit:=Sender.ClassNameIs('TMenuItem');
  if PageControl.ActivePage=MetaTabSheet then
    ShowMetaData
  else
    try
      SQLQuery.Close;
      SQLQuery.SQL.Text := SQLEditor.Text;
      SQLQuery.Open;
      PageControl.ActivePage := ResultTabSheet;
    except
      on e:Exception do
        MessageDlg(e.Message, mtError, [mbOK], 0);
    end;
end;

procedure TDBStringsPropertyEditorDlg.MetaDBGridDblClick(Sender: TObject);
begin
  if assigned(MetaDBGrid.SelectedField) and (MetaDBGrid.SelectedField.Value <> NULL) then
    if FMetaFromSynedit then
      begin
      MIPasteClick(Sender);
      end
    else
      EdtObject.Text:=MetaDBGrid.SelectedField.AsString;
end;

//-------------------------------------------------------------//
procedure TDBStringsPropertyEditorDlg.FormShow(Sender: TObject);

Var
  D : TSQLDialect;

begin
  TBCheck.Visible:=True;
  MICheck.Visible:=True;
  D:=sqlStandard;
  If Assigned(FConnection) then
    begin
    if (copy(LowerCase(FConnection.ClassName),1,3)='tib') then
      D:=sqlinterbase6
    else if (copy(LowerCase(FConnection.ClassName),1,7)='toracle') then
      D:=sqloracle
    else if (Copy(LowerCase(FConnection.ClassName),1,6)='tmysql') then
      D:=sqlmysql;
    end;
  if (CheckConnection) then
    begin
    SQLQuery.DataBase    := FConnection;
    SQLQuery.Transaction := FTransaction;
    SQLMeta.DataBase    := FConnection;
    SQLMeta.Transaction := FTransaction;
    ResultTabSheet.TabVisible    := True;
    MetaTabSheet.TabVisible    := True;
    ExecuteToolButton.Visible := True;
    FConnection.GetTableNames(SQLHighLighter.TableNames);
    end
  else
    begin
    ResultTabSheet.TabVisible    := False;
    MetaTabSheet.TabVisible    := False;
    ExecuteToolButton.Visible := False;
    end;
  SQLHighlighter.SQLDIalect:=D;
  SQLHighlighter.Enabled:=True;
  CbxMetaData.ItemIndex:=0;
{$ifdef unix}
  // keep this only because of gtk1
  {$ifdef LCLGtk}
  SQLEditor.Font.Name:='-adobe-courier-medium-r-normal-*-8-*-*-*-m-*-iso10646-1';
  {$endif}
{$endif}
  SQLEditor.SetFocus;
end;

procedure TDBStringsPropertyEditorDlg.MICleanupClick(Sender: TObject);

begin
  CleanupDelphiCode;
end;

procedure TDBStringsPropertyEditorDlg.CleanupDelphiCode;

Var
  L : TStringList;
  I,J,K : Integer;
  S : String;
begin
  L:=TStringList.Create;
  try
    L.Assign(SQLEditor.Lines);
    For I:=0 to L.Count-1 do
      begin
      S:=L[i];
      K:=0;
      For J:=1 to Length(S) do
        If S[j]='''' then
          Inc(K);
      if (K<>0) and ((K mod 2)=0) then
        begin
        J:=Pos('''',S);
        Delete(S,1,J);
        J:=RPos('''',S);
        S:=Copy(S,1,J-1);
        L[i]:=S;
        end;
      end;
    SQLEditor.Lines:=L;
  finally
    L.Free;
  end;
end;

procedure TDBStringsPropertyEditorDlg.MICreateConstantClick(Sender: TObject);
begin
  CreateConstant;
end;

procedure TDBStringsPropertyEditorDlg.MIMetaColumnsClick(Sender: TObject);
begin
  if FWordUnderCursor<>'' then
    begin
    CbxMetaData.ItemIndex:=2; //stColumns
    EdtObject.Text:=FWordUnderCursor;
    PageControl.ActivePage:=MetaTabSheet;
    ExecuteToolButtonClick(Sender);
    end;
end;

procedure TDBStringsPropertyEditorDlg.MIPasteClick(Sender: TObject);

begin
  if assigned(MetaDBGrid.SelectedField) and (MetaDBGrid.SelectedField.Value <> NULL) then
    begin
    SQLEditor.InsertTextAtCaret(' '+TEDBConnection(SQLMeta.DataBase).FieldNameQuoteChars[0]+
      trim(MetaDBGrid.SelectedField.AsString)+TEDBConnection(SQLMeta.DataBase).FieldNameQuoteChars[1]);
    PageControl.ActivePage:=EditorTabSheet;
    end;
end;

procedure TDBStringsPropertyEditorDlg.CreateConstant;

Var
  C,S : String;
  I : Integer;

begin
  C:='';
  For I:=0 to SQLEditor.Lines.Count-1 do
    begin
    S:=SQLEditor.Lines[i];
    If (C<>'') then
      C:=C+'+LineEnding+'+LineEnding;
    C:=C+''''+StringReplace(S,'''','''''',[rfReplaceAll])+'''';
    end;
  C:='SQL = '+C+';';
  Clipboard.AsText:=C;
end;

procedure TDBStringsPropertyEditorDlg.ShowMetaData;
var
  SchemaType:TSchemaType;
begin
  case CbxMetaData.ItemIndex of
    0:SchemaType:=stTables;
    2:begin
        SchemaType:=stColumns;
        if EdtObject.Text='' then
          begin
          ShowMessage(SMetaPleaseSpecifyATableInTheObjectField);
          exit;
          end;
      end;
    3:SchemaType:=stProcedures;
    1:SchemaType:=stSysTables;
    else
      SchemaType:=stNoSchema;
  end;
  if SchemaType<>stNoSchema then
    try
      SQLMeta.Close;
      SQLMeta.SetSchemaInfo(SchemaType,EdtObject.Text,'');
      SQLMeta.Open;
    except
      on e:Exception do
        MessageDlg(e.Message, mtError, [mbOK], 0);
    end;
end;

//------------------------------------------------------------------------//
procedure TDBStringsPropertyEditorDlg.SaveToolButtonClick(Sender: TObject);
begin
  if(SaveDialog.Execute)then
    SQLEditor.Lines.SaveToFile(SaveDialog.FileName);
end;

procedure TDBStringsPropertyEditorDlg.SQLEditorMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  MPos,Caret:tpoint;

begin
  If Button=mbRight then // save word under cursor
    begin
    FWordUnderCursor:='';
    MPos.x:=x;
    MPos.y:=y;
    Caret:=SQLEditor.PhysicalToLogicalPos(SQLEditor.PixelsToLogicalPos(MPos));
    FWordUnderCursor:=SQLEditor.GetWordAtRowCol(Caret);
    end;
end;

procedure TDBStringsPropertyEditorDlg.TBCheckClick(Sender: TObject);
begin
  CheckSQLSyntax(SQLEditor.Lines);
end;

procedure TDBStringsPropertyEditorDlg.TBConstClick(Sender: TObject);
begin
  CreateConstant;
end;

procedure TDBStringsPropertyEditorDlg.CheckSQLSyntax(SQL: TStrings);
Var
  S : TStream;
  P : TSQLParser;
  E : TSQLElement;
  EL : TSQLElementList;
  Msg : String;
begin
  S:=TMemoryStream.Create;
  try
    SQL.SaveToStream(S);
    S.Position:=0;
    if S.Size=0 then
      exit; // no message for empty input
    P:=TSQLParser.Create(S);
    try
      try
        If IsSQLScript then
        begin
          EL:=P.ParseScript;
          EL.Free;
        end
        else begin
          E:=P.Parse;
          E.Free;
        end;
        MessageDLG(SSQLOK,SQLSyntaxOK,mtInformation,[mbOK],0);
      except
        On E : Exception do
          begin
          Msg:=Format(SSQLSyntaxError,[E.Message]);
          MessageDLG(SSQLError,Msg,mtError,[mbOK],0);
          end;
      end;
    finally
      P.Free;
    end;
  finally
    S.Free;
  end;
end;

end.

