
{ TSQLDBFieldDefs }

class function TSQLDBFieldDefs.FieldDefClass: TFieldDefClass;
begin
  Result:=TSQLDBFieldDef;
end;


{ TSQLDBParams }

class function TSQLDBParams.ParamClass: TParamClass;
begin
  Result:=TSQLDBParam;
end;


{ ESQLDatabaseError }

constructor ESQLDatabaseError.CreateFmt(const Fmt: string; const Args: array of const;
  Comp: TComponent; AErrorCode: integer; ASQLState: string);
const CompNameFmt='%s : %s';
var Msg: string;
begin
  if not assigned(Comp) then
    Msg := Fmt
  else if Comp.Name = '' then
    Msg := Format(CompNameFmt, [Comp.ClassName,Fmt])
  else
    Msg := Format(CompNameFmt, [Comp.Name,Fmt]);

  if Length(Args) = 0 then
    inherited Create(Msg)
  else
    inherited CreateFmt(Msg, Args);

  ErrorCode := AErrorCode;
  SQLState  := ASQLState;
end;


{ TCustomSQLStatement }

procedure TCustomSQLStatement.OnChangeSQL(Sender: TObject);

var
  ConnOptions : TConnOptions;
  NewParams: TSQLDBParams;

begin
  UnPrepare;
  if not ParamCheck then
    exit;
  if assigned(DataBase) then
    ConnOptions:=DataBase.ConnOptions
  else
    ConnOptions := [sqEscapeRepeat,sqEscapeSlash];
  NewParams := CreateParams;
  try
    NewParams.ParseSQL(FSQL.Text, True, sqEscapeSlash in ConnOptions, sqEscapeRepeat in ConnOptions, psInterbase);
    NewParams.AssignValues(FParams);
    FParams.Assign(NewParams);
  finally
    NewParams.Free;
  end;
end;

procedure TCustomSQLStatement.SetDatabase(AValue: TEDBConnection);
begin
  if FDatabase=AValue then Exit;
  UnPrepare;
  If Assigned(FDatabase) then
    begin
      FDatabase.UnregisterStatement(Self);
      FDatabase.RemoveFreeNotification(Self);
    end;
  FDatabase:=AValue;
  If Assigned(FDatabase) then
    begin
      FDatabase.FreeNotification(Self);
      FDatabase.RegisterStatement(Self);
      if Assigned(Database.Transaction) and (not Assigned(Transaction) or (Transaction.DataBase <> Database)) then
        Transaction := Database.Transaction;
      OnChangeSQL(Self);
    end;
end;

procedure TCustomSQLStatement.SetCursor(AValue: TSQLCursor);
begin
  if FCursor=AValue then Exit;
  FCursor:=AValue;
end;

procedure TCustomSQLStatement.SetTransaction(AValue: TEDBTransaction);
begin
  if FTransaction=AValue then Exit;
  UnPrepare;
  if Assigned(FTransaction) then
    FTransaction.RemoveFreeNotification(Self);
  FTransaction:=AValue;
  if Assigned(FTransaction) then
    begin
    FTransaction.FreeNotification(Self);
    if Assigned(Transaction.DataBase) and (Database <> Transaction.DataBase) then
      Database := Transaction.DataBase as TEDBConnection;
    end;
end;

procedure TCustomSQLStatement.SetDataSource(AValue: TDataSource);

begin
  if GetDataSource=AValue then Exit;
  if (FDataLink=Nil) then
    FDataLink:=CreateDataLink;
  FDataLink.DataSource:=AValue;
end;

procedure TCustomSQLStatement.CopyParamsFromMaster(CopyBound: Boolean);
begin
  if Assigned(DataSource) and Assigned(DataSource.Dataset) then
    FParams.CopyParamValuesFromDataset(DataSource.Dataset,CopyBound);
end;

procedure TCustomSQLStatement.SetParams(AValue: TParams);
begin
  if FParams=AValue then Exit;
  FParams.Assign(AValue);
end;

procedure TCustomSQLStatement.SetSQL(AValue: TStrings);
begin
  if FSQL=AValue then Exit;
  FSQL.Assign(AValue);
end;

procedure TCustomSQLStatement.DoExecute;
begin
  FRowsAffected :=- 1;
  If (FParams.Count>0) and Assigned(DataSource) then
    CopyParamsFromMaster(False);
  If LogEvent(detExecute) then
    Log(detExecute,FServerSQL);
  Database.Execute(FCursor,Transaction, FParams);
end;

function TCustomSQLStatement.GetPrepared: Boolean;
begin
  Result := Assigned(FCursor) and FCursor.FPrepared;
end;

function TCustomSQLStatement.CreateDataLink: TDataLink;
begin
  Result:=TDataLink.Create;
end;

function TCustomSQLStatement.CreateParams: TSQLDBParams;
begin
  Result:=TSQLDBParams.Create(Nil);
end;

function TCustomSQLStatement.LogEvent(EventType: TDBEventType): Boolean;
begin
  Result:=Assigned(Database) and Database.LogEvent(EventType);
end;

procedure TCustomSQLStatement.Log(EventType: TDBEventType; const Msg: String);
Var
  M : String;

begin
  If LogEvent(EventType) then
    begin
    If (Name<>'') then
      M:=Name
    else
      M:=ClassName;
    Database.Log(EventType,M+' : '+Msg);
    end;
end;

procedure TCustomSQLStatement.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (operation=opRemove) then
    If (AComponent=FTransaction) then
      FTransaction:=Nil
    else if (AComponent=FDatabase) then
      begin
        UnPrepare;
        FDatabase:=Nil;
      end;
end;

constructor TCustomSQLStatement.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSQL                           := TStringList.Create;
  TStringList(FSQL).OnChange     := @OnChangeSQL;
  FParams                        := CreateParams;
  FParamCheck                    := True;
  FParseSQL                      := True;
  FRowsAffected                  := -1;
end;

destructor TCustomSQLStatement.Destroy;
begin
  UnPrepare;
  Transaction    := Nil;
  Database       := Nil;
  DataSource     := Nil;
  FreeAndNil(FDataLink);
  FreeAndNil(FParams);
  FreeAndNil(FSQL);
  inherited Destroy;
end;

function TCustomSQLStatement.GetSchemaType: TSchemaType;

begin
  Result:=stNoSchema
end;

function TCustomSQLStatement.GetSchemaObjectName: String;
begin
  Result:='';
end;

function TCustomSQLStatement.GetSchemaPattern: String;
begin
  Result:='';
end;

function TCustomSQLStatement.IsSelectable: Boolean;
begin
  Result:=False;
end;

procedure TCustomSQLStatement.GetStatementInfo(var ASQL: String; out Info: TSQLStatementInfo);

begin
  Info:=Database.GetStatementInfo(ASQL);
end;

procedure TCustomSQLStatement.AllocateCursor;

begin
  if not assigned(FCursor) then
    // Do this as late as possible.
    FCursor:=Database.AllocateCursorHandle;
end;

procedure TCustomSQLStatement.DeAllocateCursor;
begin
  if Assigned(FCursor) and Assigned(Database) then
    //DataBase.DeAllocateCursorHandle(FCursor);
end;

procedure TCustomSQLStatement.DoPrepare;

var
  StmInfo: TSQLStatementInfo;
begin
  if GetSchemaType = stNoSchema then
    FOrigSQL := TrimRight(FSQL.Text)
  else
    FOrigSQL := Database.GetSchemaInfoSQL(GetSchemaType, GetSchemaObjectName, GetSchemaPattern);
  if (FOrigSQL = '') then
    DatabaseError(SErrNoStatement);
  FServerSQL := FOrigSQL;
  GetStatementInfo(FServerSQL,StmInfo);
  AllocateCursor;
  FCursor.FSelectable     := True; // let PrepareStatement and/or Execute alter it
  FCursor.FStatementType  := StmInfo.StatementType;
  FCursor.FQueryType      := StmInfo.QueryType;
  FCursor.FSchemaType     := GetSchemaType;
  If LogEvent(detPrepare) then
    Log(detPrepare,FServerSQL);
  Database.PrepareStatement(FCursor,Transaction,FServerSQL,FParams);
end;

procedure TCustomSQLStatement.Prepare;

begin
  if Prepared then exit;
  if not assigned(Database) then
    DatabaseError(SErrDatabasenAssigned);
  if not assigned(Transaction) then
    DatabaseError(SErrTransactionnSet);
  Database.MaybeConnect;
  if not Transaction.Active then
    Transaction.MaybeStartTransaction;
  try
    DoPrepare;
  except
    DeAllocateCursor;
    Raise;
  end;
end;

procedure TCustomSQLStatement.Execute;
begin
  Prepare;
  DoExecute;
end;

procedure TCustomSQLStatement.DoUnPrepare;

begin
  If Assigned(FCursor) then
    If Assigned(Database) then
      begin
        //DataBase.UnPrepareStatement(FCursor);
        //DeAllocateCursor;
      end
    else // this should never happen. It means a cursor handle leaks in the DB itself.
      FreeAndNil(FCursor);
end;

function TCustomSQLStatement.GetDataSource: TDataSource;
begin
  if Assigned(FDataLink) then
    Result:=FDataLink.DataSource
  else
    Result:=Nil;
end;

procedure TCustomSQLStatement.Unprepare;
begin
  // Some SQLConnections does not support statement [un]preparation, but they have allocated local cursor(s)
  //  so let them do cleanup f.e. cancel pending queries and/or free resultset
  //  and also do UnRegisterStatement!
  if assigned(FCursor) then
    DoUnprepare;
end;

function TCustomSQLStatement.ParamByName(const AParamName: String): TParam;
begin
  Result:=FParams.ParamByName(AParamName);
end;

function TCustomSQLStatement.RowsAffected: TRowsCount;
begin
  if FRowsAffected=-1 then
    begin
    if Assigned(Database) then
      FRowsAffected:=Database.RowsAffected(FCursor);
    end;
  Result:=FRowsAffected;
end;


{ TEDBConnection }

constructor TEDBConnection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSQLFormatSettings   := DefaultSQLFormatSettings;
  FFieldNameQuoteChars := DoubleQuotes;
  FLogEvents           := LogAllEvents; //match Property LogEvents...Default LogAllEvents
  FStatements          := TFPList.Create;
end;

destructor TEDBConnection.Destroy;
begin
  Connected:=False; // needed because we want to de-allocate statements
  FreeAndNil(FStatements);
  inherited Destroy;
end;

function TEDBConnection.StrToStatementType(s : string) : TStatementType;
var T : TStatementType;
begin
  S:=Lowercase(s);
  for T:=stSelect to stRollback do
    if (S=StatementTokens[T]) then
      Exit(T);
  Result:=stUnknown;
end;

procedure TEDBConnection.SetTransaction(Value : TEDBTransaction);
begin
  if FTransaction<>value then
    begin
    if Assigned(FTransaction) and FTransaction.Active then
      DatabaseError(SErrAssTransaction);
    if Assigned(Value) then
      Value.Database := Self;
    FTransaction := Value;
    If Assigned(FTransaction) and (FTransaction.Database=Nil) then
      FTransaction.Database:=Self;
    end;
end;


procedure TEDBConnection.UpdateIndexDefs(IndexDefs : TIndexDefs; TableName : string);
begin
  // Empty abstract
end;

procedure TEDBConnection.DoInternalConnect;
begin
  if (DatabaseName = '') and not(sqSupportEmptyDatabaseName in FConnOptions) then
    DatabaseError(SErrNoDatabaseName,self);
end;

procedure TEDBConnection.DoInternalDisconnect;

Var
  I : integer;

begin
  For I:=0 to FStatements.Count-1 do
    TCustomSQLStatement(FStatements[i]).Unprepare;
  FStatements.Clear;
end;

procedure TEDBConnection.StartTransaction;
begin
  if not assigned(Transaction) then
    DatabaseError(SErrConnTransactionnSet)
  else
    Transaction.StartTransaction;
end;

procedure TEDBConnection.EndTransaction;
begin
  if not assigned(Transaction) then
    DatabaseError(SErrConnTransactionnSet)
  else
    Transaction.EndTransaction;
end;

procedure TEDBConnection.ExecuteDirect(SQL: String);

begin
  ExecuteDirect(SQL,FTransaction);
end;

procedure TEDBConnection.ExecuteDirect(SQL: String;
  ATransaction: TEDBTransaction);

var Cursor : TSQLCursor;

begin
  if not assigned(ATransaction) then
    DatabaseError(SErrTransactionnSet);

  if not Connected then
    Open;
  if not ATransaction.Active then
    ATransaction.MaybeStartTransaction;

  SQL := TrimRight(SQL);
  if SQL = '' then
    DatabaseError(SErrNoStatement);

  try
    Cursor := AllocateCursorHandle;
    Cursor.FStatementType := stUnknown;
    If LogEvent(detPrepare) then
      Log(detPrepare,SQL);
    PrepareStatement(Cursor,ATransaction,SQL,Nil);
    try
      If LogEvent(detExecute) then
        Log(detExecute,SQL);
      Execute(Cursor,ATransaction, Nil);
    finally
      UnPrepareStatement(Cursor);
    end;
  finally;
    DeAllocateCursorHandle(Cursor);
  end;
end;

function TEDBConnection.GetPort: cardinal;
begin
  result := StrToIntDef(Params.Values['Port'],0);
end;

procedure TEDBConnection.SetOptions(AValue: TSQLConnectionOptions);
begin
  if FOptions=AValue then Exit;
  FOptions:=AValue;
end;

procedure TEDBConnection.SetPConnection(AValue: Boolean);
begin
  if FPConnection=AValue then Exit;
  FPConnection:=AValue;
end;


procedure TEDBConnection.SetPort(const AValue: cardinal);
begin
  if AValue<>0 then
    Params.Values['Port']:=IntToStr(AValue)
  else with params do if IndexOfName('Port') > -1 then
    Delete(IndexOfName('Port'));
end;

function TEDBConnection.AttemptCommit(trans: TSQLHandle): boolean;
begin
  try
    Result:=Commit(trans);
  except
    if ForcedClose then
      Result:=True
    else
      Raise;
  end;
end;

function TEDBConnection.AttemptRollBack(trans: TSQLHandle): boolean;
begin
  try
    Result:=Rollback(trans);
  except
    if ForcedClose then
      Result:=True
    else
      Raise;
  end;
end;

procedure TEDBConnection.GetDBInfo(const ASchemaType : TSchemaType; const ASchemaObjectName, AReturnField : string; AList: TStrings);

var qry : TCustomEDBQuery;

begin
  if not assigned(Transaction) then
    DatabaseError(SErrConnTransactionnSet);

  qry := TCustomEDBQuery.Create(nil);
  try
    qry.etransaction := Transaction;
    qry.edatabase    := Self;
    with qry do
      begin
        ParseSQL := False;
        SetSchemaInfo(ASchemaType,ASchemaObjectName,'');
        open;
        AList.Clear;
        while not eof do begin
          AList.Append(trim(fieldbyname(AReturnField).asstring));
          Next;
        end;
      end;
  finally
    qry.free;
  end;
end;

function TEDBConnection.RowsAffected(cursor: TSQLCursor): TRowsCount;
begin
  Result := -1;
end;


procedure TEDBConnection.GetTableNames(List: TStrings; SystemTables: Boolean);
begin
  if not SystemTables then
    GetDBInfo(stTables,'','table_name',List)
  else
    GetDBInfo(stSysTables,'','table_name',List);
end;

procedure TEDBConnection.GetProcedureNames(List: TStrings);
begin
  GetDBInfo(stProcedures,'','procedure_name',List);
end;

procedure TEDBConnection.GetFieldNames(const TableName: string; List: TStrings);
begin
  GetDBInfo(stColumns,TableName,'column_name',List);
end;

procedure TEDBConnection.GetSchemaNames(List: TStrings);
begin
  GetDBInfo(stSchemata,'','SCHEMA_NAME',List);
end;

procedure TEDBConnection.GetSequenceNames(List: TStrings);
begin
  GetDBInfo(stSequences,'','SEQUENCE_NAME',List);
end;

{
  See if we can integrate/merge this with GetDBInfo. They are virtually identical
}

function TEDBConnection.GetObjectNames(ASchemaType: TSchemaType;
  AList: TSqlObjectIdentifierList): Integer;
var
  qry : TCustomEDBQuery;
  vSchemaName, vObjectName: String;
  f: TField;
begin
  Result:=0;
  if not assigned(Transaction) then
    DatabaseError(SErrConnTransactionnSet);

  qry := TCustomEDBQuery.Create(nil);
  try
    qry.etransaction := Transaction;
    qry.edatabase    := Self;
    with qry do
      begin
        ParseSQL := False;
        SetSchemaInfo(ASchemaType,TSchemaObjectNames[ASchemaType],'');
        open;
        f := FindField(TSchemaObjectNames[stSchemata]);
        while not eof do
          begin
            vSchemaName:='';
            if Assigned(f) then
               vSchemaName:=f.AsString;
            vObjectName:=FieldByName(FSchemaObjectName).AsString;
            AList.AddIdentifier(vObjectName, vSchemaName);
            Next;
            Inc(Result);
          end;
      end;
  finally
    qry.free;
  end;
end;

function TEDBConnection.GetConnectionInfo(InfoType: TConnInfoType): string;
var i: TConnInfoType;
begin
  Result:='';
  if InfoType = citAll then
    for i:=citServerType to citClientVersion do
      begin
      if Result<>'' then Result:=Result+',';
      Result:=Result+'"'+GetConnectionInfo(i)+'"';
      end;
end;

function TEDBConnection.GetStatementInfo(const ASQL: string): TSQLStatementInfo;

type TParsePart = (ppStart,ppWith,ppSelect,ppTableName,ppFrom,ppWhere,ppGroup,ppOrder,ppBogus);
     TPhraseSeparator = (sepNone, sepWhiteSpace, sepComma, sepComment, sepParentheses, sepDoubleQuote, sepEnd);
     TKeyword = (kwWITH, kwSELECT, kwINSERT, kwUPDATE, kwDELETE, kwFROM, kwJOIN, kwWHERE, kwGROUP, kwORDER, kwUNION, kwROWS, kwLIMIT, kwUnknown);

const
  KeywordNames: array[TKeyword] of string =
    ('WITH', 'SELECT', 'INSERT', 'UPDATE', 'DELETE', 'FROM', 'JOIN', 'WHERE', 'GROUP', 'ORDER', 'UNION', 'ROWS', 'LIMIT', '');

var
  PSQL, CurrentP, SavedP,
  PhraseP, PStatementPart : pchar;
  S                       : string;
  ParsePart               : TParsePart;
  BracketCount            : Integer;
  Separator               : TPhraseSeparator;
  Keyword, K              : TKeyword;

begin
  PSQL:=Pchar(ASQL);
  ParsePart := ppStart;

  CurrentP := PSQL-1;
  PhraseP := PSQL;

  Result.TableName := '';
  Result.Updateable := False;
  Result.WhereStartPos := 0;
  Result.WhereStopPos := 0;

  repeat
    begin
      inc(CurrentP);
      SavedP := CurrentP;

      case CurrentP^ of
        ' ', #9..#13:
          Separator := sepWhiteSpace;
        ',':
          Separator := sepComma;
        #0, ';':
          Separator := sepEnd;
        '(':
          begin
            Separator := sepParentheses;
            // skip everything between brackets, since it could be a sub-select, and
            // further nothing between brackets could be interesting for the parser.
            BracketCount := 1;
            repeat
              inc(CurrentP);
              if CurrentP^ = '(' then
                 inc(BracketCount)
              else if CurrentP^ = ')'
                 then dec(BracketCount);
            until (CurrentP^ = #0) or (BracketCount = 0);
            if CurrentP^ <> #0 then inc(CurrentP);
          end;
        '"','`':
          if SkipComments(CurrentP, sqEscapeSlash in ConnOptions, sqEscapeRepeat in ConnOptions) then
            Separator := sepDoubleQuote;
        else
          if SkipComments(CurrentP, sqEscapeSlash in ConnOptions, sqEscapeRepeat in ConnOptions) then
            Separator := sepComment
          else
            Separator := sepNone;
      end;

    if (CurrentP > SavedP) and (SavedP > PhraseP) then
      CurrentP := SavedP;  // there is something before comment or left parenthesis

    if Separator <> sepNone then
      begin
        if ((Separator in [sepWhitespace,sepComment]) and (PhraseP = SavedP)) then
          PhraseP := CurrentP;  // skip comments (but not parentheses) and white spaces

      if (CurrentP-PhraseP > 0) or (Separator = sepEnd) then
        begin
          SetString(s, PhraseP, CurrentP-PhraseP);

          Keyword := kwUnknown;
          for K in TKeyword do
            if SameText(s, KeywordNames[K]) then
            begin
              Keyword := K;
              break;
            end;

          case ParsePart of
            ppStart  :
                      begin
                         Result.StatementType := StrToStatementType(s);
                         case Keyword of
                           kwWITH  : ParsePart := ppWith;
                           kwSELECT: ParsePart := ppSelect;
                           else      break;
                         end;
                      end;
            ppWith   :
                     begin
                       // WITH [RECURSIVE] CTE_name [ ( column_names ) ] AS ( CTE_query_definition ) [, ...]
                       //  { SELECT | INSERT | UPDATE | DELETE } ...
                       case Keyword of
                         kwSELECT: Result.StatementType := stSelect;
                         kwINSERT: Result.StatementType := stInsert;
                         kwUPDATE: Result.StatementType := stUpdate;
                         kwDELETE: Result.StatementType := stDelete;
                       end;
                       if Result.StatementType <> stUnknown then break;
                     end;
            ppSelect :
                     begin
                       if Keyword = kwFROM then
                         ParsePart := ppTableName;
                     end;
            ppTableName:
                    begin
                       // Meta-data requests are never updateable
                       //  and select statements from more than one table
                       //  and/or derived tables are also not updateable
                       if Separator in [sepWhitespace, sepComment, sepDoubleQuote, sepEnd] then
                         begin
                           Result.TableName  := s;
                           Result.Updateable := True;
                           Result.QueryType  := qtSingle;
                         end;
                       ParsePart := ppFrom;
                    end;
            ppFrom   :
                    begin
                       if (Keyword in [kwWHERE, kwGROUP, kwORDER, kwLIMIT, kwROWS]) or
                          (Separator = sepEnd) then
                         begin
                           case Keyword of
                               kwWHERE: ParsePart := ppWhere;
                               kwGROUP: ParsePart := ppGroup;
                               kwORDER: ParsePart := ppOrder;
                             else
                               ParsePart := ppBogus;
                           end;
                           Result.WhereStartPos := PhraseP-PSQL+1;
                           PStatementPart := CurrentP;
                         end
                       else // joined table or user_defined_function (...)
                       if (Keyword = kwJOIN) or (Separator in [sepComma, sepParentheses]) then
                         begin
                           Result.TableName  := Result.TableName;   // Original ''
                           Result.Updateable := Result.Updateable;  // Original False
                           Result.QueryType  := qtComposite;
                         end;
                    end;
            ppWhere  :
                    begin
                       if (Keyword in [kwGROUP, kwORDER, kwLIMIT, kwROWS]) or
                          (Separator = sepEnd) then
                         begin
                           ParsePart := ppBogus;
                           Result.WhereStartPos := PStatementPart-PSQL;
                           if (Separator = sepEnd) then
                             Result.WhereStopPos := CurrentP-PSQL+1
                           else
                             Result.WhereStopPos := PhraseP-PSQL+1;
                         end
                       else if (Keyword = kwUNION) then
                         begin
                           ParsePart := ppBogus;
                           Result.Updateable := False;
                         end;
                    end;
          end; {case}
        end;
        if Separator in [sepComment, sepParentheses, sepDoubleQuote] then
          dec(CurrentP);
        PhraseP := CurrentP+1;
      end
    end;
  until CurrentP^=#0;
end;

function TEDBConnection.GetAsSQLText(Field : TField) : string;
begin
  if (not assigned(Field)) or Field.IsNull then
    Result := 'Null'
  else
    case field.DataType of
      ftString   : Result := QuotedStr(Field.AsString);
      ftDate     : Result := '''' + FormatDateTime('yyyy-mm-dd',Field.AsDateTime,FSqlFormatSettings) + '''';
      ftDateTime : Result := '''' + FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',Field.AsDateTime,FSqlFormatSettings) + '''';
      ftTime     : Result := QuotedStr(TimeIntervalToString(Field.AsDateTime));
    else
      Result := Field.AsString;
    end; {case}
end;

function TEDBConnection.GetAsSQLText(Param: TParam) : string;
begin
  if (not assigned(Param)) or Param.IsNull then Result := 'Null'
  else case Param.DataType of
    ftGuid,
    ftMemo,
    ftFixedChar,
    ftString   : Result := QuotedStr(Param.AsString);
    ftDate     : Result := '''' + FormatDateTime('yyyy-mm-dd',Param.AsDateTime,FSQLFormatSettings) + '''';
    ftTime     : Result := QuotedStr(TimeIntervalToString(Param.AsDateTime));
    ftDateTime : Result := '''' + FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Param.AsDateTime, FSQLFormatSettings) + '''';
    ftCurrency,
    ftBcd      : Result := CurrToStr(Param.AsCurrency, FSQLFormatSettings);
    ftFloat    : Result := FloatToStr(Param.AsFloat, FSQLFormatSettings);
    ftFMTBcd   : Result := stringreplace(Param.AsString, DefaultFormatSettings.DecimalSeparator, FSQLFormatSettings.DecimalSeparator, []);
  else
    Result := Param.AsString;
  end; {case}
end;


function TEDBConnection.GetHandle: pointer;
begin
  Result := nil;
end;

function TEDBConnection.LogEvent(EventType: TDBEventType): Boolean;
begin
  Result:=(Assigned(FOnLog) or Assigned(GlobalDBLogHook)) and (EventType in LogEvents);
end;

procedure TEDBConnection.LogParams(const AParams: TParams);

Var
  S : String;
  P : TParam;

begin
  if not LogEvent(detParamValue) or not Assigned(AParams) then
    Exit;
  For P in AParams do
    begin
    if P.IsNull then
      S:='<NULL>'
    else if (P.DataType in ftBlobTypes) and  not (P.DataType in [ftMemo, ftFmtMemo,ftWideMemo]) then
      S:='<BLOB>'
    else
      S:=P.AsString;
    Log(detParamValue,Format(SLogParamValue,[P.Name,S]));
    end;
end;

procedure TEDBConnection.Log(EventType: TDBEventType; const Msg: String);

Var
  M : String;

begin
  If LogEvent(EventType) then
    begin
    If Assigned(FonLog) then
      FOnLog(Self,EventType,Msg);
    If Assigned(GlobalDBLogHook) then
      begin
      If (Name<>'') then
        M:=Name+' : '+Msg
      else
        M:=ClassName+' : '+Msg;
        GlobalDBLogHook(Self,EventType,M);
      end;
    end;
end;

procedure TEDBConnection.RegisterStatement(S: TCustomSQLStatement);
begin
  if FStatements.IndexOf(S)=-1 then
    FStatements.Add(S);
end;

procedure TEDBConnection.UnRegisterStatement(S: TCustomSQLStatement);
begin
  if Assigned(FStatements) then // Can be nil, when we are destroying and datasets are uncoupled.
    FStatements.Remove(S);
end;


function TEDBConnection.InitialiseUpdateStatement(Query : TCustomEDBQuery; var qry : TCustomEDBQuery): TCustomEDBQuery;

begin
  if not assigned(qry) then
  begin
    qry                := TCustomEDBQuery.Create(nil);
    qry.ParseSQL       := False;
    qry.EDataBase      := Self;
    qry.ETransaction   := Query.SQLTransaction;
    qry.Unidirectional := True;
    qry.UsePrimaryKeyAsKey := False;
    qry.PacketRecords      := 1;
  end;
  Result:=qry;
end;


procedure TEDBConnection.AddFieldToUpdateWherePart(var sql_where : string;UpdateMode : TUpdateMode; F : TField);

begin
  if (pfInKey in F.ProviderFlags)
     or ((UpdateMode = upWhereAll) and (pfInWhere in F.ProviderFlags))
     or ((UpdateMode = UpWhereChanged) and (pfInWhere in F.ProviderFlags) and (F.Value <> F.OldValue)) then
     begin
     if (sql_where<>'') then
       sql_where:=sql_where + ' and ';
     sql_where:= sql_where + '(' + FieldNameQuoteChars[0] + F.FieldName + FieldNameQuoteChars[1];
     if F.OldValue = NULL then
        sql_where :=  sql_where + ' is null '
     else
        sql_where :=  sql_where +'= :"' + 'OLD_' + F.FieldName + '"';
     sql_where:=sql_where+') ';
     end;
end;


function TEDBConnection.ConstructInsertSQL(Query: TCustomEDBQuery;
  var ReturningClause: Boolean): string;

var x          : integer;
    sql_fields : string;
    sql_values : string;
    returning_fields : String;
    F : TField;
begin
  sql_fields := '';
  sql_values := '';
  returning_fields := '';
  for x := 0 to Query.Fields.Count -1 do
    begin
    F:=Query.Fields[x];
    if (not F.IsNull) and (pfInUpdate in F.ProviderFlags) and (not F.ReadOnly) then
      begin
      sql_fields := sql_fields + FieldNameQuoteChars[0] + F.FieldName + FieldNameQuoteChars[1] + ',';
      sql_values := sql_values + ':"' + F.FieldName + '",';
      end;
    if ReturningClause and (pfRefreshOnInsert in F.ProviderFlags) then
      returning_fields := returning_fields + FieldNameQuoteChars[0] + F.FieldName + FieldNameQuoteChars[1] + ',';
    end;
  if length(sql_fields) = 0 then
    DatabaseErrorFmt(sNoUpdateFields,['insert'],self);
  setlength(sql_fields,length(sql_fields)-1);
  setlength(sql_values,length(sql_values)-1);
  result := 'insert into ' + Query.FTableName + ' (' + sql_fields + ') values (' + sql_values + ')';
  if ReturningClause then
    begin
    ReturningClause:=length(returning_fields) <> 0 ;
    if ReturningClause then
      begin
      setlength(returning_fields,length(returning_fields)-1);
      Result := Result + ' returning ' + returning_fields;
      end;
    end;
end;


function TEDBConnection.ConstructUpdateSQL(Query: TCustomEDBQuery;
  var ReturningClause: Boolean): string;

var x : integer;
    F : TField;
    sql_set    : string;
    sql_where  : string;
    returning_fields : String;
begin
  sql_set    := '';
  sql_where  := '';
  returning_fields := '';
  for x := 0 to Query.Fields.Count -1 do
    begin
      F:=Query.Fields[x];
      AddFieldToUpdateWherePart(sql_where,Query.UpdateMode,F);
      if (pfInUpdate in F.ProviderFlags) and (not F.ReadOnly) then
        sql_set := sql_set +FieldNameQuoteChars[0] + F.FieldName + FieldNameQuoteChars[1] +'=:"' + F.FieldName + '",';
      if ReturningClause and (pfRefreshOnUpdate in F.ProviderFlags) then
        returning_fields := returning_fields + FieldNameQuoteChars[0] + F.FieldName + FieldNameQuoteChars[1] + ',';
    end;
  if length(sql_set) = 0 then DatabaseErrorFmt(sNoUpdateFields,['update'],self);
  setlength(sql_set,length(sql_set)-1);
  if length(sql_where) = 0 then DatabaseErrorFmt(sNoWhereFields,['update'],self);
  result := 'update ' + Query.FTableName + ' set ' + sql_set + ' where ' + sql_where;
  if ReturningClause then
    begin
    ReturningClause:=length(returning_fields) <> 0 ;
    if ReturningClause then
      begin
      setlength(returning_fields,length(returning_fields)-1);
      Result := Result + ' returning ' + returning_fields;
      end;
    end;
end;


function TEDBConnection.ConstructDeleteSQL(Query : TCustomEDBQuery) :  string;

var
  x          : integer;
  sql_where  : string;

begin
  sql_where := '';
  for x := 0 to Query.Fields.Count -1 do
    AddFieldToUpdateWherePart(sql_where,Query.UpdateMode, Query.Fields[x]);
  if length(sql_where) = 0 then
    DatabaseErrorFmt(sNoWhereFields,['delete'],self);
  result := 'delete from ' + Query.FTableName + ' where ' + sql_where;
end;

function TEDBConnection.ConstructRefreshSQL(Query: TCustomEDBQuery; UpdateKind: TUpdateKind): string;

Var
  F : TField;
  PF : TProviderFlag;
  Where : String;
begin
  Result := Query.RefreshSQL.Text;
  if (Result='') then
    begin
      Where := '';
      PF    := RefreshFlags[UpdateKind];
      For F in Query.Fields do
        begin
          if PF in F.ProviderFlags then
            begin
              if (Result<>'') then
                Result:= Result+', ';
              if (F.Origin<>'') and (F.Origin<>F.FieldName) then
                Result := Result + F.Origin + ' AS '  + F.FieldName
              else
                Result:= Result + FieldNameQuoteChars[0] + F.FieldName + FieldNameQuoteChars[1]
            end;
          if pfInkey in F.ProviderFlags then
            begin
              if (Where<>'') then
                Where := Where + ' AND ';
              Where := Where + '('+FieldNameQuoteChars[0] + F.FieldName + FieldNameQuoteChars[1]+' = :'+F.FieldName+')';
            end;
        end;
      if (Where='') then
        DatabaseError(SErrNoKeyFieldForRefreshClause,Query);
      Result:='SELECT '+Result+' FROM '+Query.FTableName+' WHERE '+Where;
    end;
end;

procedure TEDBConnection.ApplyFieldUpdate(C : TSQLCursor; P : TSQLDBParam;F : TField; UseOldValue : Boolean);

begin
  if UseOldValue then
    P.AssignFieldValue(F,F.OldValue)
  else
    P.AssignFieldValue(F,F.Value);
  P.FFieldDef:=F.FieldDef;
end;

procedure TEDBConnection.ApplyRecUpdate(Query: TCustomEDBQuery; UpdateKind: TUpdateKind);

var
  qry : TCustomEDBQuery;
  s   : string;
  x   : integer;
  Fld : TField;
  P : TParam;
  B,ReturningClause : Boolean;

begin
  qry := Nil;
  ReturningClause:=(sqSupportReturning in ConnOptions) and not (sqoRefreshUsingSelect in Query.Options) and (Query.RefreshSQL.Count=0);
  case UpdateKind of
    ukInsert : begin
                 s := Trim(Query.FInsertSQL.Text);
                 if s = '' then
                   s := ConstructInsertSQL(Query, ReturningClause)
                 else
                   ReturningClause := False;
                 qry := InitialiseUpdateStatement(Query, Query.FInsertQry);
               end;
    ukModify : begin
                 s := Trim(Query.FUpdateSQL.Text);
                 if s = '' then
                     s := ConstructUpdateSQL(Query, ReturningClause)
                 else
                   ReturningClause := False;
                 qry := InitialiseUpdateStatement(Query, Query.FUpdateQry);
              end;
    ukDelete : begin
                 s := Trim(Query.FDeleteSQL.Text);
                 if (s='') and (not assigned(Query.FDeleteQry) or (Query.UpdateMode<>upWhereKeyOnly)) then
                   s := ConstructDeleteSQL(Query);
                 ReturningClause := False;
                 qry := InitialiseUpdateStatement(Query, Query.FDeleteQry);
               end;
  end;
  if (s<>'') and (qry.SQL.Text<>s) then
    qry.SQL.Text:=s; //assign only when changed, to avoid UnPrepare/Prepare
  Assert(qry.SQL.Text<>'');
  for x := 0 to Qry.Params.Count-1 do
    begin
      P  := Qry.Params[x];
      S  := p.name;
      B  := SameText(leftstr(S,4),'OLD_');
      if B then
        Delete(S,1,4);
      Fld := Query.FieldByName(S);
      ApplyFieldUpdate(Query.Cursor,P as TSQLDBParam,Fld,B);
    end;
  if ReturningClause then
    begin
      Qry.Close;
      Qry.Open
    end
  else
    Qry.Execute;
  if (scoApplyUpdatesChecksRowsAffected in Options) and (Qry.RowsAffected<>1) then
    begin
      Qry.Close;
      DatabaseErrorFmt(SErrFailedToUpdateRecord, [Qry.RowsAffected], Query);
    end;
  if ReturningClause then
    Query.ApplyReturningResult(Qry,UpdateKind);
end;

{function TEDBConnection.RefreshLastInsertID(Query: TCustomEDBQuery; Field: TField): Boolean;
begin
  Result:=False;
end;}

procedure TEDBConnection.FreeFldBuffers(cursor: TSQLCursor);
begin
  // empty
end;

function TEDBConnection.StartImplicitTransaction(trans: TSQLHandle; aParams: string): boolean;
begin
  Result:=False;
end;

function TEDBConnection.GetSchemaInfoSQL( SchemaType : TSchemaType; SchemaObjectName, SchemaPattern : string) : string;

begin
  case SchemaType of
    stProcedures: Result := 'SELECT *, ROUTINE_NAME AS PROCEDURE_NAME FROM INFORMATION_SCHEMA.ROUTINES';
    stSchemata  : Result := 'SELECT * FROM INFORMATION_SCHEMA.SCHEMATA';
    stSequences : Result := 'SELECT * FROM INFORMATION_SCHEMA.SEQUENCES';
    else DatabaseError(SMetadataUnavailable);
  end;
end;

function TEDBConnection.GetNextValueSQL(const SequenceName: string; IncrementBy: Integer): string;
begin
  Result := 'SELECT NEXT VALUE FOR ' + SequenceName;
end;

function TEDBConnection.GetNextValue(const SequenceName: string; IncrementBy: integer): Int64;
var
  Q: TCustomEDBQuery;
begin
  Result := 0;
  Q := TCustomEDBQuery.Create(nil);
  try
    Q.EDataBase      := Self;
    Q.ETransaction   := Transaction;
    Q.SQL.Text       := GetNextValueSQL(SequenceName, IncrementBy);
    Q.Open;
    if not Q.Eof then
      Result := Q.Fields[0].AsLargeInt;
    Q.Close;
  finally
    FreeAndNil(Q);
  end;
end;

procedure TEDBConnection.CheckConnection;
begin
  MaybeConnect;
end;

procedure TEDBConnection.MaybeConnect;
begin
  If Not Connected then
    begin
      If (scoExplicitConnect in Options) then
        DatabaseErrorFmt(SErrImplicitConnect,[Name]);
      Connected := True;
    end;
end;

procedure TEDBConnection.CreateDB;

begin
  DatabaseError(SNotSupported);
end;

procedure TEDBConnection.DropDB;

begin
  DatabaseError(SNotSupported);
end;


{ TEDBTransaction }

constructor TEDBTransaction.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FParams := TStringList.Create;
  Action := caRollBack;
end;

destructor TEDBTransaction.Destroy;
begin
  EndTransaction;
  FreeAndNil(FTrans);
  FreeAndNil(FParams);
  inherited Destroy;
end;

procedure TEDBTransaction.EndTransaction;

begin
  Case Action of
    caCommit, caCommitRetaining :
      Commit;
    caNone,
    caRollback, caRollbackRetaining :
      if not (stoUseImplicit in Options) then
        RollBack
      else
        CloseTrans;
  end;
end;

procedure TEDBTransaction.SetParams(const AValue: TStringList);
begin
  FParams.Assign(AValue);
end;

function TEDBTransaction.GetSQLConnection: TEDBConnection;
begin
  Result:=Database as TEDBConnection;
end;

procedure TEDBTransaction.SetOptions(AValue: TSQLTransactionOptions);
begin
  if FOptions=AValue then Exit;
  if (stoUseImplicit in Avalue) and Assigned(SQLConnection) And Not (sqImplicitTransaction in SQLConnection.ConnOptions) then
    DatabaseErrorFmt(SErrNoImplicitTransaction, [SQLConnection.ClassName]);
  FOptions:=AValue;
end;

procedure TEDBTransaction.SetSQLConnection(AValue: TEDBConnection);
begin
  Database:=AValue;
end;

Procedure TEDBTransaction.MaybeStartTransaction;
begin
  if not Active then
    begin
    if (stoExplicitStart in Options) then
      DatabaseErrorFmt(SErrImplictTransactionStart, [Database.Name,Name]);
    StartTransaction;
    end;
end;

function TEDBTransaction.GetHandle: Pointer;
begin
  Result := SQLConnection.GetTransactionHandle(FTrans);
end;

Function TEDBTransaction.AllowClose(DS: TDBDataset): Boolean;
begin
  if (DS is TEDBQuery) then
    Result:=not (sqoKeepOpenOnCommit in TEDBQuery(DS).Options)
  else
    Result:=Inherited AllowClose(DS);
end;

procedure TEDBTransaction.Commit;
begin
  if Active  then
    begin
      //CloseDataSets;
      If LogEvent(detCommit) then
        Log(detCommit,SCommitting);
      if (stoUseImplicit in Options) or SQLConnection.AttemptCommit(FTrans) then
        begin
          CloseTrans;
          FreeAndNil(FTrans);
        end;
    end;
end;

procedure TEDBTransaction.CommitRetaining;
begin
  if Active then
    begin
    If LogEvent(detCommit) then
      Log(detCommit,SCommitRetaining);
    SQLConnection.CommitRetaining(FTrans);
    end;
end;

procedure TEDBTransaction.Rollback;
begin
  if Active then
    begin
    if (stoUseImplicit in Options) then
      DatabaseError(SErrImplicitNoRollBack);
      //CloseDataSets;
    If LogEvent(detRollback) then
      Log(detRollback,SRollingBack);
    if SQLConnection.AttemptRollBack(FTrans) then
      begin
        CloseTrans;
        FreeAndNil(FTrans);
      end;
    end;
end;

procedure TEDBTransaction.RollbackRetaining;
begin
  if Active then
    begin
    if (stoUseImplicit in Options) then
      DatabaseError(SErrImplicitNoRollBack);
    If LogEvent(detRollback) then
      Log(detRollback,SRollBackRetaining);
    SQLConnection.RollBackRetaining(FTrans);
    end;
end;

procedure TEDBTransaction.StartTransaction;

var db : TEDBConnection;

begin
  if Active then
    DatabaseError(SErrTransAlreadyActive);

  db := SQLConnection;

  if Db = nil then
    DatabaseError(SErrDatabasenAssigned);

  Db.MaybeConnect;

  if not assigned(FTrans) then FTrans := Db.AllocateTransactionHandle;

  if (stoUseImplicit in Options) then
    begin
    if Db.StartImplicitTransaction(FTrans,FParams.CommaText) then
      OpenTrans
    end
  else
    begin
    if Db.StartDBTransaction(FTrans,FParams.CommaText) then
      OpenTrans
    end;
end;

Procedure TEDBTransaction.SetDatabase(Value: TDatabase);

begin
  If Value<>Database then
    begin
    if Assigned(Value) and not (Value is TEDBConnection) then
      DatabaseErrorFmt(SErrNotASQLConnection, [Value.Name], Self);
    CheckInactive;
    if (stoUseImplicit in Options) and Assigned(Value) and Not (sqImplicitTransaction in TEDBConnection(Value).ConnOptions) then
      DatabaseErrorFmt(SErrNoImplicitTransaction, [Value.ClassName]);
    If Assigned(Database) then
      if SQLConnection.Transaction = Self then SQLConnection.Transaction := nil;
    inherited;
    If Assigned(Database) and not (csLoading in ComponentState) then
      If SQLConnection.Transaction = Nil then SQLConnection.Transaction := Self;
    end;
end;

Function TEDBTransaction.LogEvent(EventType: TDBEventType): Boolean;
begin
  Result:=Assigned(Database) and SQLConnection.LogEvent(EventType);
end;

Procedure TEDBTransaction.Log(EventType: TDBEventType; Const Msg: String);

Var
  M : String;

begin
  If LogEvent(EventType) then
    begin
    If (Name<>'') then
      M:=Name+' : '+Msg
    else
      M:=Msg;
    SQLConnection.Log(EventType,M);
    end;
end;


{ TSQLSequence }

constructor TSQLSequence.Create(AQuery: TCustomEDBQuery);
begin
  inherited Create;
  FQuery := AQuery;
  FApplyEvent := saeOnNewRecord;
  FIncrementBy := 1;
end;

procedure TSQLSequence.Assign(Source: TPersistent);
var SourceSequence: TSQLSequence;
begin
  if Source is TSQLSequence then
  begin
    SourceSequence := TSQLSequence(Source);
    FFieldName    := SourceSequence.FieldName;
    FSequenceName := SourceSequence.SequenceName;
    FIncrementBy  := SourceSequence.IncrementBy;
    FApplyEvent   := SourceSequence.ApplyEvent;
  end
  else
    inherited;
end;

procedure TSQLSequence.Apply;
var Field: TField;
begin
  if Assigned(FQuery) and (FSequenceName<>'') and (FFieldName<>'') then
  begin
    Field := FQuery.FindField(FFieldName);
    if Assigned(Field) and Field.IsNull then
      Field.AsLargeInt := GetNextValue;
  end;
end;

function TSQLSequence.GetNextValue: Int64;
begin
  if (FQuery=Nil) or (FQuery.SQLConnection=Nil) then
    DatabaseError(SErrDatabasenAssigned);
  Result := FQuery.SQLConnection.GetNextValue(FSequenceName, FIncrementBy);
end;


Type

  { TQuerySQLStatement }

  TQuerySQLStatement = Class(TCustomSQLStatement)
  protected
    FQuery : TCustomEDBQuery;
    Function CreateDataLink : TDataLink; override;
    Function GetSchemaType : TSchemaType; override;
    Function GetSchemaObjectName : String; override;
    Function GetSchemaPattern: String; override;
    procedure GetStatementInfo(var ASQL: String; out Info: TSQLStatementInfo); override;
    procedure OnChangeSQL(Sender : TObject); override;
  end;

{ TQuerySQLStatement }

function TQuerySQLStatement.CreateDataLink: TDataLink;
begin
  Result := TMasterParamsDataLink.Create(FQuery);
end;

function TQuerySQLStatement.GetSchemaType: TSchemaType;
begin
  if Assigned(FQuery) then
    Result := FQuery.FSchemaType
  else
    Result := stNoSchema;
end;

function TQuerySQLStatement.GetSchemaObjectName: String;
begin
  if Assigned(FQuery) then
    Result := FQuery.FSchemaObjectname
  else
    Result := inherited GetSchemaObjectName;
end;

function TQuerySQLStatement.GetSchemaPattern: String;
begin
  if Assigned(FQuery) then
    Result:=FQuery.FSchemaPattern
  else
    Result:=inherited GetSchemaPattern;
end;

procedure TQuerySQLStatement.GetStatementInfo(var ASQL: String; out Info: TSQLStatementInfo);
begin
  inherited GetStatementInfo(ASQL, Info);
  If Assigned(FQuery) then
    // Note: practical side effect of switch off ParseSQL is that UpdateServerIndexDefs is bypassed
    //       which is used as performance tuning option
    if (FQuery.FSchemaType = stNoSchema) and FParseSQL then
      begin
      FQuery.FUpdateable   := Info.Updateable;
      FQuery.FTableName    := Info.TableName;
      FQuery.FWhereStartPos:= Info.WhereStartPos;
      FQuery.FWhereStopPos := Info.WhereStopPos;
      if FQuery.ServerFiltered then
        ASQL:=FQuery.AddFilter(ASQL);
      end
    else
      begin
        FQuery.FUpdateable     := false;
        FQuery.FTableName      := '';
        FQuery.FWhereStartPos  := 0;
        FQuery.FWhereStopPos   := 0;
      end;
end;

procedure TQuerySQLStatement.OnChangeSQL(Sender: TObject);
begin
  UnPrepare;
  inherited OnChangeSQL(Sender);
  If ParamCheck and Assigned(FDataLink) then
    (FDataLink as TMasterParamsDataLink).RefreshParamNames;
  FQuery.ServerIndexDefs.Updated:=false;
end;

{ TCustomEDBQuery }

constructor TCustomEDBQuery.Create(AOwner : TComponent);

Var
  F : TQuerySQLStatement;

begin
  inherited Create(AOwner);
  F            := TQuerySQLStatement.Create(Self);
  F.FQuery     := Self;
  FStatement   := F;

  FInsertSQL          := TStringList.Create;
  FInsertSQL.OnChange := @OnChangeModifySQL;
  FUpdateSQL          := TStringList.Create;
  FUpdateSQL.OnChange := @OnChangeModifySQL;
  FDeleteSQL          := TStringList.Create;
  FDeleteSQL.OnChange := @OnChangeModifySQL;
  FRefreshSQL         := TStringList.Create;
  FRefreshSQL.OnChange := @OnChangeModifySQL;

  FSequence            := TSQLSequence.Create(Self);
  FServerIndexDefs     := TServerIndexDefs.Create(Self);

  FServerFiltered      := False;
  FServerFilterText    := '';

  FSchemaType          := stNoSchema;
  FSchemaObjectName    := '';
  FSchemaPattern       := '';

// Delphi has upWhereAll as default, but since strings and oldvalue's don't work yet
// (variants) set it to upWhereKeyOnly
  FUpdateMode := upWhereKeyOnly;
  FUsePrimaryKeyAsKey := True;
end;

destructor TCustomEDBQuery.Destroy;
begin
  if Active then Close;
  UnPrepare;
  FreeAndNil(FStatement);
  FreeAndNil(FInsertSQL);
  FreeAndNil(FUpdateSQL);
  FreeAndNil(FDeleteSQL);
  FreeAndNil(FRefreshSQL);
  FreeAndNil(FSequence);
  FreeAndNil(FServerIndexDefs);
  EDatabase    := Nil;
  ETransaction := Nil;
  inherited Destroy;
end;

function TCustomEDBQuery.ParamByName(const AParamName: String): TParam;

begin
  Result := Params.ParamByName(AParamName);
end;

procedure TCustomEDBQuery.OnChangeModifySQL(Sender : TObject);

begin
  CheckInactive;
end;

function TCustomEDBQuery.IsPrepared: Boolean;

begin
  if Assigned(Fstatement) then
    Result := FStatement.Prepared
  else
    Result := False;
end;

function TCustomEDBQuery.AddFilter(SQLstr: string): string;

begin
  if (FWhereStartPos > 0) and (FWhereStopPos > 0) then
    begin
      system.insert('(',SQLstr,FWhereStartPos+1);
      system.insert(')',SQLstr,FWhereStopPos+1);
    end;

  if FWhereStartPos = 0 then
    SQLstr := SQLstr + ' where (' + ServerFilter + ')'
  else if FWhereStopPos > 0 then
    system.insert(' and ('+ServerFilter+') ',SQLstr,FWhereStopPos+2)
  else
    system.insert(' where ('+ServerFilter+') ',SQLstr,FWhereStartPos);
  Result := SQLstr;
end;

function TCustomEDBQuery.NeedRefreshRecord(UpdateKind: TUpdateKind): Boolean;


Var
  PF : TProviderFlag;
  I : Integer;
  DoReturning : Boolean;

begin
  Result:=(FRefreshSQL.Count<>0);
  DoReturning:=(sqSupportReturning in SQLConnection.ConnOptions) and not (sqoRefreshUsingSelect in Options);
  if Not (Result or DoReturning) then
    begin
      PF := RefreshFlags[UpdateKind];
      I  := 0;
      While (Not Result) and (I<Fields.Count) do
        begin
          Result:=PF in Fields[i].ProviderFlags;
          Inc(I);
        end;
    end;
end;

function TCustomEDBQuery.RefreshRecord(UpdateKind: TUpdateKind): Boolean;

Var
  Q : TCustomEDBQuery;
  P : TParam;
  F,FD : TField;
  N : String;

begin
  Result := False;
  Q      := TCustomEDBQuery.Create(Nil);
  try
    Q.EDatabase    := Self.EDatabase;
    Q.ETransaction := Self.ETransaction;
    Q.SQL.Text     := SQLConnection.ConstructRefreshSQL(Self,UpdateKind);
    For P in Q.Params do
      begin
        N:=P.Name;
        If CompareText(Copy(N,1,4),'OLD_')=0 then
          system.Delete(N,1,4);
        F:=Fields.FindField(N);
        if Assigned(F) then
          P.AssignField(F);
      end;
    Q.Open;
    try
      if (Q.EOF and Q.BOF) then
        DatabaseError(SErrRefreshEmptyResult,Self)
      else
        begin
          if Q.RecordCount<>1 then
            DatabaseErrorFmt(SErrRefreshNotSingleton,[Q.RecordCount],Self);
          For F in Q.Fields do
            begin
              FD:=Fields.FindField(F.FieldName);
              if Assigned(FD) then
                begin
                  FD.Assign(F);
                  Result:=True; // We could check if the new value differs from the old, but we won't.
                end;
            end;
        end
    finally
      Q.Close;
    end;
  finally
    Q.Free;
  end;
end;

procedure TCustomEDBQuery.ApplyReturningResult(Q: TCustomEDBQuery; UpdateKind : TUpdateKind);

Var
  S : TDataSetState;
  refreshFlag  : TProviderFlag;
  F : TField;
begin
  RefreshFlag := RefreshFlags[UpdateKind];
  S           := SetTempState(dsRefreshFields);
  try
    For F in Fields do
      if RefreshFlag in F.ProviderFlags then
        F.Assign(Q.FieldByName(F.FieldName));
  finally
    RestoreState(S);
  end;
end;

procedure TCustomEDBQuery.ApplyFilter;

begin
  FreeFldBuffers;
  FStatement.Unprepare;
  FIsEOF  := False;
  inherited InternalClose;
  FStatement.DoPrepare;
  FStatement.DoExecute;
  inherited InternalOpen;
  First;
end;

procedure TCustomEDBQuery.SetActive(Value: Boolean);

begin
  inherited SetActive(Value);
// The query is UnPrepared, so that if a transaction closes all datasets
// they also get unprepared
  if not Value and IsPrepared then begin
    UnPrepare;
  end;
end;


procedure TCustomEDBQuery.SetServerFiltered(Value: Boolean);

begin
  if Value and not ParseSQL then
    DatabaseErrorFmt(SNoParseSQL,['Filtering ']);
  if (ServerFiltered <> Value) then
    begin
      FServerFiltered := Value;
      if Active then
         ApplyFilter;
    end;
end;

procedure TCustomEDBQuery.SetServerFilterText(const Value: string);
begin
  if Value <> ServerFilter then
    begin
      FServerFilterText := Value;
      if Active then ApplyFilter;
    end;
end;


procedure TCustomEDBQuery.Prepare;

begin
  FStatement.Prepare;
  if Assigned(FStatement.FCursor) then
    with FStatement.FCursor do
      FInitFieldDef := FSelectable;
end;

procedure TCustomEDBQuery.UnPrepare;

begin
  CheckInactive;
  If Assigned(FStatement) then
    FStatement.Unprepare;
end;

procedure TCustomEDBQuery.FreeFldBuffers;
begin
  if assigned(Cursor) then
     SQLConnection.FreeFldBuffers(Cursor);
end;

function TCustomEDBQuery.GetParamCheck: Boolean;
begin
  Result:=FStatement.ParamCheck;
end;

function TCustomEDBQuery.GetParams: TParams;
begin
  Result:=FStatement.Params;
end;

function TCustomEDBQuery.GetParseSQL: Boolean;
begin
  Result:=FStatement.ParseSQL;
end;

function TCustomEDBQuery.GetQueryType: TQueryType;
begin
  if Assigned(Cursor) then
    Result := Cursor.FQueryType
  else
    Result := qtSingle;
end;

function TCustomEDBQuery.GetServerIndexDefs: TServerIndexDefs;
begin
  Result := FServerIndexDefs;
end;

function TCustomEDBQuery.GetSQL: TStringList;
begin
  Result:=TStringList(Fstatement.SQL);
end;

function TCustomEDBQuery.GetSQLConnection: TEDBConnection;
begin
  Result:=EDatabase as TEDBConnection;
end;

function TCustomEDBQuery.GetSQLTransaction: TEDBTransaction;
begin
  Result:=ETransaction as TEDBTransaction;
end;

function TCustomEDBQuery.Cursor: TSQLCursor;
begin
  Result := FStatement.Cursor;
end;

function TCustomEDBQuery.Fetch : boolean;
begin
  if Not Assigned(Cursor) then
    Exit;
  if not Cursor.FSelectable then
    Exit;
  If LogEvent(detFetch) then
    Log(detFetch,FStatement.FServerSQL);
  SQLConnection.CheckConnection;
  if not FIsEof then FIsEOF := not SQLConnection.Fetch(Cursor);
  Result := not FIsEOF;
end;

procedure TCustomEDBQuery.Execute;
begin
  FStatement.Execute;
end;

function TCustomEDBQuery.RowsAffected: TRowsCount;
begin
  Result := FStatement.RowsAffected;
end;

function TCustomEDBQuery.LoadField(FieldDef : TFieldDef; buffer : pointer; out CreateBlob : boolean) : boolean;
begin
  Result := SQLConnection.LoadField(Cursor, FieldDef, buffer, CreateBlob);
  // disable deferred blob loading for "disconnected" datasets
  if Result and (FieldDef.DataType in ftBlobTypes) and (sqoKeepOpenOnCommit in Options) then
    CreateBlob:=True
end;

procedure TCustomEDBQuery.LoadBlobIntoBuffer(FieldDef: TFieldDef;
  ABlobBuf: PBufBlobField);
begin
  SQLConnection.LoadBlobIntoBuffer(FieldDef, ABlobBuf, Cursor,SQLTransaction);
end;

procedure TCustomEDBQuery.InternalAddRecord(Buffer: Pointer; AAppend: Boolean);
begin
  // not implemented - sql dataset
end;

procedure TCustomEDBQuery.InternalClose;
begin
  if assigned(Cursor) then
    begin
      if Cursor.FSelectable then
        FreeFldBuffers;
      // Some SQLConnections does not support statement [un]preparation,
      //  so let them do cleanup f.e. cancel pending queries and/or free resultset
      if not Prepared then
        FStatement.DoUnprepare;
    end;

  if DefaultFields then
    DestroyFields;

  FIsEOF := False;
  if assigned(FUpdateQry) then FreeAndNil(FUpdateQry);
  if assigned(FInsertQry) then FreeAndNil(FInsertQry);
  if assigned(FDeleteQry) then FreeAndNil(FDeleteQry);
//  FRecordSize := 0;

  inherited InternalClose;
end;

procedure TCustomEDBQuery.InternalInitFieldDefs;
begin
  if FLoadingFieldDefs then
    Exit;

  FLoadingFieldDefs := True;

  try
    FieldDefs.Clear;
    Prepare;
    SQLConnection.AddFieldDefs(Cursor,FieldDefs);
  finally
    FLoadingFieldDefs := False;
    if assigned(Cursor) then Cursor.FInitFieldDef := False;
  end;
end;

procedure TCustomEDBQuery.InternalOpen;

var counter, fieldc : integer;
    F               : TField;
    IndexFields     : TStrings;
begin
  if IsReadFromPacket then
    begin
    // When we read from file there is no need for Cursor, also note that Database may not be assigned
    //FStatement.AllocateCursor;
    //Cursor.FSelectable:=True;
    //Cursor.FStatementType:=stSelect;
      FUpdateable := True;
    end
  else
    begin
      Prepare;
      if not Cursor.FSelectable then
        DatabaseError(SErrNoSelectStatement,Self);

      // Call UpdateServerIndexDefs before Execute, to avoid problems with connections
      // which do not allow processing multiple recordsets at a time. (Microsoft
      // calls this MARS, see bug 13241)
      if DefaultFields and FUpdateable and FusePrimaryKeyAsKey and (not IsUniDirectional) then
        UpdateServerIndexDefs;

      Execute;
      if not Cursor.FSelectable then
        DatabaseError(SErrNoSelectStatement,Self);

      // InternalInitFieldDef is only called after a prepare. i.e. not twice if
      // a dataset is opened - closed - opened.
      if Cursor.FInitFieldDef then
        InternalInitFieldDefs;
      if DefaultFields then
        begin
          CreateFields;
        if FUpdateable and FusePrimaryKeyAsKey and (not IsUniDirectional) then
          for counter := 0 to ServerIndexDefs.Count-1 do
            if ixPrimary in ServerIndexDefs[counter].Options then
              begin
                IndexFields := TStringList.Create;
                ExtractStrings([';'],[' '],pchar(ServerIndexDefs[counter].Fields),IndexFields);
                for fieldc := 0 to IndexFields.Count-1 do
                  begin
                  F := FindField(IndexFields[fieldc]);
                  if F <> nil then
                    F.ProviderFlags := F.ProviderFlags + [pfInKey];
                  end;
                IndexFields.Free;
              end;
        end;
    end;
  BindFields(True);

  if not ReadOnly and not FUpdateable and (FSchemaType=stNoSchema) then
    begin
    if (trim(FDeleteSQL.Text) <> '') or (trim(FUpdateSQL.Text) <> '') or
       (trim(FInsertSQL.Text) <> '') then FUpdateable := True;
    end;

  inherited InternalOpen;
end;

procedure TCustomEDBQuery.InternalRefresh;
begin
  if (ChangeCount>0) and (sqoCancelUpdatesOnRefresh in Options) then
    CancelUpdates;
  inherited InternalRefresh;
end;

// public part

procedure TCustomEDBQuery.ExecSQL;
begin
  try
    Prepare;
    Execute;
    If sqoAutoCommit in Options then
      begin
        // Retrieve rows affected
        FStatement.RowsAffected;
        SQLTransaction.Commit;
      end;
  finally
    // Cursor has to be assigned, or else the prepare went wrong before PrepareStatment was
    //   called, so UnPrepareStatement shoudn't be called either
    // Don't deallocate cursor; f.e. RowsAffected is requested later
    if not Prepared and (assigned(EDatabase)) and (assigned(Cursor)) then
      SQLConnection.UnPrepareStatement(Cursor);
  end;
end;

procedure TCustomEDBQuery.ApplyUpdates(MaxErrors: Integer);
begin
  inherited ApplyUpdates(MaxErrors);
  If sqoAutoCommit in Options then
    begin
      // Retrieve rows affected for last update.
      FStatement.RowsAffected;
      SQLTransaction.Commit;
    end;
end;

procedure TCustomEDBQuery.Post;
begin
  inherited Post;
  If (sqoAutoApplyUpdates in Options) then
    ApplyUpdates;
end;

procedure TCustomEDBQuery.Delete;
begin
  inherited Delete;
  If (sqoAutoApplyUpdates in Options) then
    ApplyUpdates;
end;

function TCustomEDBQuery.SortDataSet(DataSet: TCustomEDBQuery;
  const FieldName: String): Boolean;
var
  i     : Integer;
  objIndexDefs: TIndexDefs;
  cIndexName: String;
  IndexOptions: TIndexOptions;
  Field: TField;
begin
  Result := False;
  Field := DataSet.Fields.FindField(FieldName);
  //If invalid field name, exit.
  if Field = nil then Exit;
  //if invalid field type, exit.
  if (Field is TBlobField) or (Field is TVariantField)
     or (Field is TBinaryField) then Exit;
  //Get IndexDefs and IndexName using RTTI
  if IsPublishedProp(DataSet, 'IndexDefs') then
    objIndexDefs := GetObjectProp(DataSet, 'IndexDefs') as TIndexDefs
  else
    Exit;
  if IsPublishedProp(DataSet, 'IndexName') then
    cIndexName := GetStrProp(DataSet, 'IndexName')
  else
    Exit;
  //Ensure IndexDefs is up-to-date
  objIndexDefs.Update;
  //If an ascending index is already in use,
  //switch to a descending index
  if cIndexName = FieldName + '__IdxA'
  then
    begin
      cIndexName := FieldName + '__IdxD';
      IndexOptions := [ixDescending];
    end
  else
    begin
      cIndexName := FieldName + '__IdxA';
      IndexOptions := [];
    end;
  //Look for existing index
  for i := 0 to Pred(objIndexDefs.Count) do
  begin
    if objIndexDefs[i].Name = cIndexName then
      begin
        Result := True;
        Break
      end;  //if
  end; // for
  //If existing index not found, create one
  if not Result then
      begin
        if cIndexName=FieldName + '__IdxD' then
          DataSet.AddIndex(cIndexName, FieldName, IndexOptions, FieldName)
        else
          DataSet.AddIndex(cIndexName, FieldName, IndexOptions);
        Result := True;
      end; // if not
  //Set the index
  SetStrProp(DataSet, 'IndexName', cIndexName);
end;

procedure TCustomEDBQuery.SetReadOnly(AValue : Boolean);

begin
  CheckInactive;
  inherited SetReadOnly(AValue);
end;

procedure TCustomEDBQuery.SetParseSQL(AValue : Boolean);

begin
  CheckInactive;
  FStatement.ParseSQL:=AValue;
  if not AValue then
    FServerFiltered := False;
end;

procedure TCustomEDBQuery.SetSQL(const AValue: TStringList);
begin
  FStatement.SQL.Assign(AValue);
end;

procedure TCustomEDBQuery.SetUsePrimaryKeyAsKey(AValue : Boolean);

begin
  if not Active then
    FusePrimaryKeyAsKey := AValue
  else
    begin
       // Just temporary, this should be possible in the future
       DatabaseError(SActiveDataset);
    end;
end;

procedure TCustomEDBQuery.UpdateServerIndexDefs;

begin
  FServerIndexDefs.Clear;
  if assigned(EDataBase) and (FTableName<>'') then
    SQLConnection.UpdateIndexDefs(ServerIndexDefs,FTableName);
end;

function TCustomEDBQuery.NeedLastInsertID: TField;

Var
  I : Integer;

begin
  Result:=Nil;
  if sqLastInsertID in SQLConnection.ConnOptions then
    begin
      I:=0;
      While (Result=Nil) and (I<Fields.Count) do
        begin
          Result := Fields[i];
          if (Result.DataType<>ftAutoInc) or not Result.IsNull then
            Result:=Nil;
          Inc(I);
        end;
    end
end;

procedure TCustomEDBQuery.SetEDatabase(AValue: TDatabase);
var
  DB : TEDBConnection;
begin
  if FEDatabase = AValue then Exit;
  CheckInactive;
  FEDatabase := AValue;
  if Assigned(AValue) and not (AValue is TEDBConnection) then
    DatabaseErrorFmt(SErrNotASQLConnection, [AValue.Name], Self);
  UnPrepare;
  DB := TEDBConnection(AValue);
 If Assigned(FStatement) then
    FStatement.Database := DB;
  if Assigned(DB) and Assigned(DB.Transaction) and (not Assigned(ETransaction) or (ETransaction.DataBase<>EDatabase)) then
    ETransaction := DB.Transaction;
end;

procedure TCustomEDBQuery.SetETransaction(AValue: TDBTransaction);
begin
  CheckInactive;
  if FETransaction =  AValue then Exit;
  FETransaction    := AValue;
  UnPrepare;
  If Assigned(FStatement) then
    FStatement.Transaction := TEDBTransaction(AValue);
  If Assigned(ETransaction) and Assigned(ETransaction.DataBase) and (EDatabase<>ETransaction.DataBase) then
    EDatabase := ETransaction.Database;
end;

function TCustomEDBQuery.RefreshLastInsertID(Field: TField): Boolean;
begin
  Result:= SQLConnection.RefreshLastInsertID(Self, Field);
end;

procedure TCustomEDBQuery.ApplyRecUpdate(UpdateKind: TUpdateKind);

Var
  DoRefresh : Boolean;
  LastIDField : TField;
  S : TDataSetState;

begin
  // Moved to connection: the SQLConnection always has more information about types etc.
  // than SQLQuery itself.
  SQLConnection.ApplyRecUpdate(Self,UpdateKind);

  if UpdateKind = ukInsert then
    LastIDField := NeedLastInsertID
  else
    LastIDField :=nil;
  DoRefresh := (UpdateKind in [ukModify,ukInsert]) and NeedRefreshRecord(UpdateKind);
  if assigned(LastIDField) or DoRefresh then
    begin
      // updates fields directly in record buffer of TBufDataSet
      //   TDataSet buffers are resynchronized at end of ApplyUpdates process
      S := SetTempState(dsRefreshFields);
      try
        if assigned(LastIDField) then
          RefreshLastInsertID(LastIDField);
        if DoRefresh then
          RefreshRecord(UpdateKind);
      finally
        RestoreState(S);
      end;
    end;
end;

procedure TCustomEDBQuery.SetPacketRecords(aValue: integer);
begin
  if (AValue=PacketRecords) then exit;
  if (AValue<>-1) and (sqoKeepOpenOnCommit in Options) then
    DatabaseError(SErrDisconnectedPacketRecords);
  Inherited SetPacketRecords(aValue);
end;


function TCustomEDBQuery.GetCanModify: Boolean;
begin
  // the test for assigned(Cursor) is needed for the case that the dataset isn't opened
  if assigned(Cursor) and (Cursor.FStatementType = stSelect) then
    begin
      if (Cursor.FQueryType = qtSingle) then
         Result:= FUpdateable and (not ReadOnly) and (not IsUniDirectional)
      else
        Result:= FUpdateable and (not ReadOnly) and (not IsUniDirectional)
        and (FUpdateCompositeQuery);
    end
  else
    Result := False;
end;

procedure TCustomEDBQuery.SetUpdateMode(AValue : TUpdateMode);

begin
  FUpdateMode := AValue;
end;

procedure TCustomEDBQuery.SetSchemaInfo( ASchemaType : TSchemaType; ASchemaObjectName, ASchemaPattern : string);

begin
  FSchemaType       := ASchemaType;
  FSchemaObjectName := ASchemaObjectName;
  FSchemaPattern    := ASchemaPattern;
end;

procedure TCustomEDBQuery.BeforeRefreshOpenCursor;
begin
  // This is only necessary because TIBConnection can not re-open a
  // prepared cursor. In fact this is wrong, but has never led to
  // problems because in SetActive(false) queries are always
  // unprepared. (which is also wrong, but has to be fixed later)
  if IsPrepared then
    with SQLConnection do
         UnPrepareStatement(Cursor);
end;

function TCustomEDBQuery.LogEvent(EventType: TDBEventType): Boolean;
begin
  Result:=Assigned(EDatabase) and SQLConnection.LogEvent(EventType);
end;

procedure TCustomEDBQuery.Log(EventType: TDBEventType; const Msg: String);

Var
  M : String;

begin
  If LogEvent(EventType) then
    begin
      M:=Msg;
      If (Name<>'') then
        M:=Name+' : '+M;
      SQLConnection.Log(EventType,M);
    end;
end;

class function TCustomEDBQuery.FieldDefsClass: TFieldDefsClass;
begin
  Result:=TSQLDBFieldDefs;
end;

function TCustomEDBQuery.GetStatementType : TStatementType;

begin
  if Assigned(Cursor) then
    Result:=Cursor.FStatementType
  else
    Result:=stUnknown;
end;

procedure TCustomEDBQuery.SetParamCheck(AValue: Boolean);
begin
  FStatement.ParamCheck:=AValue;
end;

procedure TCustomEDBQuery.SetOptions(AValue: TSQLQueryOptions);
begin
  if FOptions=AValue then Exit;
  CheckInactive;
  FOptions:=AValue;
  if sqoKeepOpenOnCommit in FOptions then
    PacketRecords:=-1;
end;

procedure TCustomEDBQuery.SetSQLConnection(AValue: TEDBConnection);
begin
  EDatabase:=AValue;
end;

procedure TCustomEDBQuery.SetSQLTransaction(AValue: TEDBTransaction);
begin
  ETransaction:=AValue;
end;

procedure TCustomEDBQuery.SetInsertSQL(const AValue: TStringList);
begin
  FInsertSQL.Assign(AValue);
end;

procedure TCustomEDBQuery.SetUpdateCompositeQuery(AValue: Boolean);
begin
  if FUpdateCompositeQuery=AValue then Exit;
  FUpdateCompositeQuery:=AValue;
end;

procedure TCustomEDBQuery.SetUpdateSQL(const AValue: TStringList);
begin
  FUpdateSQL.Assign(AValue);
end;

procedure TCustomEDBQuery.SetDeleteSQL(const AValue: TStringList);
begin
  FDeleteSQL.Assign(AValue);
end;

procedure TCustomEDBQuery.SetRefreshSQL(const AValue: TStringList);
begin
  FRefreshSQL.Assign(AValue);
end;


procedure TCustomEDBQuery.SetParams(AValue: TParams);
begin
  FStatement.Params.Assign(AValue);
end;

procedure TCustomEDBQuery.SetDataSource(AValue: TDataSource);

Var
  DS : TDataSource;

begin
  DS:=DataSource;
  If (AValue<>DS) then
    begin
    If Assigned(AValue) and (AValue.Dataset=Self) then
      DatabaseError(SErrCircularDataSourceReferenceNotAllowed,Self);
    If Assigned(DS) then
      DS.RemoveFreeNotification(Self);
    FStatement.DataSource:=AValue;
    end;
end;

function TCustomEDBQuery.GetDataSource: TDataSource;

begin
  If Assigned(FStatement) then
    Result:=FStatement.DataSource
  else
    Result:=Nil;
end;

procedure TCustomEDBQuery.Notification(AComponent: TComponent; Operation: TOperation);

begin
  Inherited;
  If (Operation=opRemove) and (AComponent=DataSource) then
    DataSource:=Nil;
end;

procedure TCustomEDBQuery.DoOnNewRecord;
begin
  inherited;
  if FSequence.ApplyEvent = saeOnNewRecord then
    FSequence.Apply;
end;

procedure TCustomEDBQuery.DoBeforePost;
begin
  if (State = dsInsert) and (FSequence.ApplyEvent = saeOnPost) then
    FSequence.Apply;
  inherited;
end;

function TCustomEDBQuery.PSGetUpdateException(E: Exception; Prev: EUpdateError): EUpdateError;
var
  PrevErrorCode, ErrorCode: Integer;
begin
  if Assigned(Prev) then
    PrevErrorCode := Prev.ErrorCode
  else
    PrevErrorCode := 0;

  if E is ESQLDatabaseError then
    ErrorCode := ESQLDatabaseError(E).ErrorCode
  else
    ErrorCode := 0;

  Result := EUpdateError.Create(SOnUpdateError, E.Message, ErrorCode, PrevErrorCode, E);
end;

function TCustomEDBQuery.PSGetTableName: string;
begin
  Result := FTableName;
end;

{ TEDBScript }

procedure TEDBScript.ExecuteStatement(SQLStatement: TStrings;
  var StopExecution: Boolean);
begin
  fquery.SQL.assign(SQLStatement);
  fquery.ExecSQL;
end;

procedure TEDBScript.ExecuteDirective(Directive, Argument: String;
  var StopExecution: Boolean);
begin
  if assigned (FOnDirective) then
    FOnDirective (Self, Directive, Argument, StopExecution);
end;

procedure TEDBScript.ExecuteCommit(CommitRetaining: boolean=true);
begin
  if FTransaction is TEDBTransaction then
    if CommitRetaining then
      TEDBTransaction(FTransaction).CommitRetaining
    else
      begin
      TEDBTransaction(FTransaction).Commit;
      TEDBTransaction(FTransaction).StartTransaction;
      end
  else
    begin
    FTransaction.Active := false;
    FTransaction.Active := true;
    end;
end;

procedure TEDBScript.SetDatabase(Value: TDatabase);
begin
  FDatabase := Value;
end;

procedure TEDBScript.SetTransaction(Value: TDBTransaction);
begin
  FTransaction := Value;
end;

procedure TEDBScript.CheckDatabase;
begin
  If (FDatabase=Nil) then
    DatabaseError(SErrNoDatabaseAvailable,Self)
end;

constructor TEDBScript.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FQuery := TCustomEDBQuery.Create(nil);
  FQuery.ParamCheck := false; // Do not parse for parameters; breaks use of e.g. select bla into :bla in Firebird procedures
end;

destructor TEDBScript.Destroy;
begin
  FQuery.Free;
  inherited Destroy;
end;

procedure TEDBScript.Execute;
begin
  FQuery.DataBase := FDatabase;
  FQuery.Transaction := FTransaction;
  inherited Execute;
end;

procedure TEDBScript.ExecuteScript;
begin
  Execute;
end;


{ Connection definitions }

Var
  ConnDefs : TStringList;

Procedure CheckDefs;

begin
  If (ConnDefs=Nil) then
    begin
    ConnDefs:=TStringList.Create;
    ConnDefs.Sorted:=True;
    ConnDefs.Duplicates:=dupError;
    end;
end;

Procedure DoneDefs;

Var
  I : Integer;


begin
  If Assigned(ConnDefs) then
    begin
    For I:=ConnDefs.Count-1 downto 0 do
      begin
      ConnDefs.Objects[i].Free;
      ConnDefs.Delete(I);
      end;
    FreeAndNil(ConnDefs);
    end;
end;


Function GetConnectionDef(ConnectorName : String) : TConnectionDef;

Var
  I : Integer;

begin
  CheckDefs;
  I:=ConnDefs.IndexOf(ConnectorName);
  If (I<>-1) then
    Result:=TConnectionDef(ConnDefs.Objects[i])
  else
    Result:=Nil;
end;

procedure RegisterConnection(Def: TConnectionDefClass);

Var
  I : Integer;

begin
  CheckDefs;
  I:=ConnDefs.IndexOf(Def.TypeName);
  If (I=-1) then
    ConnDefs.AddObject(Def.TypeName,Def.Create)
  else
    begin
    ConnDefs.Objects[I].Free;
    ConnDefs.Objects[I]:=Def.Create;
    end;
end;

procedure UnRegisterConnection(Def: TConnectionDefClass);
begin
  UnRegisterConnection(Def.TypeName);
end;

procedure UnRegisterConnection(ConnectionName: String);

Var
  I : Integer;

begin
  if (ConnDefs<>Nil) then
    begin
    I:=ConnDefs.IndexOf(ConnectionName);
    If (I<>-1) then
      begin
      ConnDefs.Objects[I].Free;
      ConnDefs.Delete(I);
      end;
    end;
end;

procedure GetConnectionList(List: TSTrings);
begin
  CheckDefs;
  List.Text:=ConnDefs.Text;
end;

{ TSQLConnector }

procedure TEDBConnector.SetConnectorType(const AValue: String);
begin
  if FConnectorType<>AValue then
    begin
    CheckDisconnected;
    If Assigned(FProxy) then
      FreeProxy;
    FConnectorType:=AValue;
    CreateProxy;
    end;
end;

procedure TEDBConnector.SetTransaction(Value: TEDBTransaction);
begin
  inherited SetTransaction(Value);
  If Assigned(FProxy) and (FProxy.Transaction<>Value) then
    FProxy.FTransaction:=Value;
end;

procedure TEDBConnector.DoInternalConnect;

Var
  D : TConnectionDef;

begin
  inherited DoInternalConnect;
  CheckProxy;
  FProxy.CharSet       := Self.CharSet;
  FProxy.DatabaseName  := Self.DatabaseName;
  FProxy.HostName      := Self.HostName;
  FProxy.LogEvents     := Self.LogEvents;
  FProxy.Password      := Self.Password;
  FProxy.Role          := Self.Role;
  FProxy.UserName      := Self.UserName;
  FProxy.FTransaction  := Self.Transaction;
  FProxy.LogEvents     := Self.LogEvents;
  FProxy.OnLog         := Self.OnLog;
  FProxy.Options       := Self.Options;
  D                    := GetConnectionDef(ConnectorType);
  D.ApplyParams(Params,FProxy);
  FProxy.Connected     := True;
end;

procedure TEDBConnector.DoInternalDisconnect;
begin
  FProxy.Connected:=False;
  inherited DoInternalDisconnect;
end;

procedure TEDBConnector.CheckProxy;
begin
  If (FProxy=Nil) then
    CreateProxy;
end;

procedure TEDBConnector.CreateProxy;

Var
  D : TConnectionDef;

begin
  D:=GetConnectionDef(ConnectorType);
  If (D=Nil) then
    DatabaseErrorFmt(SErrUnknownConnectorType,[ConnectorType],Self);
  FProxy               := D.ConnectionClass.Create(Self);
  FFieldNameQuoteChars := FProxy.FieldNameQuoteChars;
  FConnOptions         := FProxy.ConnOptions;
end;

procedure TEDBConnector.FreeProxy;
begin
  FProxy.Connected:=False;
  FreeAndNil(FProxy);
end;

function TEDBConnector.StrToStatementType(s: string): TStatementType;
begin
  CheckProxy;
  Result:=FProxy.StrToStatementType(s);
end;

function TEDBConnector.GetAsSQLText(Field: TField): string;
begin
  CheckProxy;
  Result:=FProxy.GetAsSQLText(Field);
end;

function TEDBConnector.GetAsSQLText(Param: TParam): string;
begin
  CheckProxy;
  Result:=FProxy.GetAsSQLText(Param);
end;

function TEDBConnector.GetHandle: pointer;
begin
  CheckProxy;
  Result:=FProxy.GetHandle;
end;

function TEDBConnector.RefreshLastInsertID(Query: TCustomEDBQuery; Field: TField
  ): Boolean;
begin
  CheckProxy;
  Result := FProxy.RefreshLastInsertID(Query,Field);
end;

function TEDBConnector.AllocateCursorHandle: TSQLCursor;
begin
  CheckProxy;
  Result:=FProxy.AllocateCursorHandle;
end;

procedure TEDBConnector.DeAllocateCursorHandle(var cursor: TSQLCursor);
begin
  CheckProxy;
  FProxy.DeAllocateCursorHandle(cursor);
end;

function TEDBConnector.AllocateTransactionHandle: TSQLHandle;
begin
  CheckProxy;
  Result:=FProxy.AllocateTransactionHandle;
end;

procedure TEDBConnector.PrepareStatement(cursor: TSQLCursor;
  ATransaction: TEDBTransaction; buf: string; AParams: TParams);
begin
  CheckProxy;
  FProxy.PrepareStatement(cursor, ATransaction, buf, AParams);
end;

procedure TEDBConnector.Execute(cursor: TSQLCursor;
  atransaction: TEDBTransaction; AParams: TParams);
begin
  CheckProxy;
  FProxy.Execute(cursor, atransaction, AParams);
end;

function TEDBConnector.Fetch(cursor: TSQLCursor): boolean;
begin
  CheckProxy;
  Result:=FProxy.Fetch(cursor);
end;

procedure TEDBConnector.AddFieldDefs(cursor: TSQLCursor; FieldDefs: TfieldDefs
  );
begin
  CheckProxy;
  FProxy.AddFieldDefs(cursor, FieldDefs);
end;

procedure TEDBConnector.UnPrepareStatement(cursor: TSQLCursor);
begin
  CheckProxy;
  FProxy.UnPrepareStatement(cursor);
end;

procedure TEDBConnector.FreeFldBuffers(cursor: TSQLCursor);
begin
  CheckProxy;
  FProxy.FreeFldBuffers(cursor);
end;

function TEDBConnector.LoadField(cursor: TSQLCursor; FieldDef: TFieldDef;
  buffer: pointer; out CreateBlob: boolean): boolean;
begin
  CheckProxy;
  Result:=FProxy.LoadField(cursor, FieldDef, buffer, CreateBlob);
end;

procedure TEDBConnector.LoadBlobIntoBuffer(FieldDef: TFieldDef;
  ABlobBuf: PBufBlobField; cursor: TSQLCursor; ATransaction: TEDBTransaction);
begin
  CheckProxy;
  FProxy.LoadBlobIntoBuffer(FieldDef, ABlobBuf, cursor, ATransaction);
end;

function TEDBConnector.RowsAffected(cursor: TSQLCursor): TRowsCount;
begin
  CheckProxy;
  Result := FProxy.RowsAffected(cursor);
end;

function TEDBConnector.GetTransactionHandle(trans: TSQLHandle): pointer;
begin
  CheckProxy;
  Result:=FProxy.GetTransactionHandle(trans);
end;

function TEDBConnector.Commit(trans: TSQLHandle): boolean;
begin
  CheckProxy;
  Result:=FProxy.Commit(trans);
end;

function TEDBConnector.RollBack(trans: TSQLHandle): boolean;
begin
  CheckProxy;
  Result:=FProxy.RollBack(trans);
end;

function TEDBConnector.StartDBTransaction(trans: TSQLHandle; aParams: string): boolean;
begin
  CheckProxy;
  Result:=FProxy.StartDBTransaction(trans, aParams);
end;

procedure TEDBConnector.CommitRetaining(trans: TSQLHandle);
begin
  CheckProxy;
  FProxy.CommitRetaining(trans);
end;

procedure TEDBConnector.RollBackRetaining(trans: TSQLHandle);
begin
  CheckProxy;
  FProxy.RollBackRetaining(trans);
end;

procedure TEDBConnector.UpdateIndexDefs(IndexDefs: TIndexDefs;
  TableName: string);
begin
  CheckProxy;
  FProxy.UpdateIndexDefs(IndexDefs, TableName);
end;

function TEDBConnector.GetSchemaInfoSQL(SchemaType: TSchemaType;
  SchemaObjectName, SchemaPattern: string): string;
begin
  CheckProxy;
  Result:=FProxy.GetSchemaInfoSQL(SchemaType, SchemaObjectName, SchemaPattern);
end;

function TEDBConnector.GetLastInsertId: Integer;
begin
  CheckProxy;
  Result := FProxy.GetLastInsertId;
end;


{ TConnectionDef }

class function TConnectionDef.TypeName: String;
begin
  Result:='';
end;

class function TConnectionDef.ConnectionClass: TSQLConnectionClass;
begin
  Result:=Nil;
end;

class function TConnectionDef.Description: String;
begin
  Result:='';
end;

class function TConnectionDef.DefaultLibraryName: String;
begin
  Result:='';
end;

class function TConnectionDef.LoadFunction: TLibraryLoadFunction;
begin
  Result:=Nil;
end;

class function TConnectionDef.UnLoadFunction: TLibraryUnLoadFunction;
begin
  Result:=Nil;
end;

class function TConnectionDef.LoadedLibraryName: string;
begin
  Result:='';
end;

procedure TConnectionDef.ApplyParams(Params: TStrings;
  AConnection: TEDBConnection);
begin
  AConnection.Params.Assign(Params);
end;

{ TServerIndexDefs }

constructor TServerIndexDefs.create(ADataset: TDataset);
begin
  if not (ADataset is TCustomEDBQuery) then
    DatabaseErrorFmt(SErrNotASQLQuery,[ADataset.Name]);
  inherited create(ADataset);
end;

procedure TServerIndexDefs.Update;
begin
  if (not updated) and assigned(Dataset) then
    begin
         TCustomEDBQuery(Dataset).UpdateServerIndexDefs;
         updated := True;
    end;
end;
