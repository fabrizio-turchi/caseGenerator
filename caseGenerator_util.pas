unit caseGenerator_util;

interface
uses
  System.Classes, System.SysUtils, System.StrUtils;

function ExtractField(line, subLine: String): String;
function ExtractRefID(line, subline: String): String;
function ExtractDateValue(line, subline: String): String;
function ExtractNumericValue(line, subline: String): String;
function ExtractArrayRefID(line, subline: String): TArray<String>;
function ExtractDataField(line, subLine: String): String;
function ExtractArray(line, subline: String): TStringList;
function ExtractArrayID(line, subline: String): TStringList;
function RepeatString(const s: String; count: cardinal): String;
function SearchItemList(itemList: String; list:TStringList): Integer;
function CountOccurrences(substringText, stringText: String): Integer;

implementation


function ExtractRefId(line, subline: String): String;
var
  posId: Integer;
  uuid: String;
begin
  posId := Pos(subline, line);
  uuid := Copy(line, posId + Length(subline) + 1, Length(line));
  Result := ExtractField(uuid, '@id":"');
end;

function ExtractDateValue(line, subline: String): String;
var
  posId: Integer;
  uuid: String;
begin
  posId := Pos(subline, line);
  uuid := Copy(line, posId + Length(subline) + 1, Length(line));
  Result := ExtractField(uuid, '"@value":"');
end;

function ExtractNumericValue(line, subline: String): String;
var
  posId, posValue, posRecSep: Integer;
  lineValue, numValue, recSep: String;
begin
  recSep := #30 + #30;
  posId := Pos(subline, line);
  lineValue := Copy(line, posId + Length(subline) + 1, Length(line));
  posValue := Pos('"@value":', lineValue);
  numValue  := Copy(lineValue, posValue + Length('"@value":'), Length(lineValue));
  posRecSep  := Pos(recSep, numValue);
  Result := Copy(numValue, 1, posRecSep - 1);
end;

function ExtractField(line, subLine: String): String;
var
  fieldValue, recSep: String;
  fieldStart, fieldEnd: Integer;
begin
  recSep := #30 + #30;
  fieldStart := Pos(subLine, line); // search pos of subLine inside line
  if fieldStart = 0 then // not found
    Result := ''
  else
  begin
    fieldValue := Copy(line, fieldStart + Length(subLine), Length(line));
    fieldValue := stringreplace(fieldValue, recSep, '',[rfReplaceAll]);
    fieldEnd   := Pos('"', fieldValue);
    Result := Copy(fieldValue, 1, fieldEnd - 1);
  end;

end;

function ExtractDataField(line, subLine: String): String;
var
  fieldValue, dataValue, recSep: String;
  fieldStart, fieldEnd: Integer;
begin
  recSep := #30 + #30;
  fieldStart := Pos(subLine, line); // search pos of subLine inside line
  if fieldStart = 0 then // not found
    Result := ''
  else
  begin
    dataValue := Copy(line, fieldStart + Length(subLine), Length(line));
    fieldStart := Pos('"@value":"', dataValue); // search pos of data/time value inside dataValue
    if fieldStart = 0 then // not found
      Result := ''
    else
    begin
      fieldValue := Copy(dataValue, fieldStart + Length('"@value":"'), Length(dataValue));
      fieldValue := stringreplace(fieldValue, recSep, '',[rfReplaceAll]);
      fieldEnd   := Pos('"', fieldValue);
      Result := Copy(fieldValue, 1, fieldEnd - 1);
    end;
  end;

end;

function SearchItemList(itemList: String; list: TStringList): Integer;
var
i: integer;
begin
  Result := -1;
  for i:=0 to list.Count - 1 do
  begin
    if AnsiContainsStr(list[i], itemList) then
    begin
      Result := i;
      exit;
    end;

  end;

end;

function ExtractArray(line, subLine: String): TStringList;
var
  fieldValue, recSep, indent: String;
  fieldStart, fieldEnd: Integer;
  itemsArray: String;
  commaPos: Integer;
  itemsList: TStringList;
begin
  recSep := #30 + #30;
  indent := '   ';
  fieldStart := Pos(subLine, line); // search pos of subLine inside line
  fieldValue := Copy(line, fieldStart + Length(subLine), Length(line));
  fieldValue := stringreplace(fieldValue, recSep, '',[rfReplaceAll]);
  fieldValue := stringreplace(fieldValue, '"', '',[rfReplaceAll]);
  fieldValue := stringreplace(fieldValue, indent, '',[rfReplaceAll]);
  fieldEnd   := Pos(']', fieldValue);
  itemsArray := Copy(fieldValue, 1, fieldEnd - 1);
  itemsList := TStringList.Create;
  commaPos := Pos(',', itemsArray);
  while commaPos > 0 do
  begin
    itemsList.Add(Copy(itemsArray, 1, commaPos - 1));
    itemsArray := Copy(itemsArray, commaPos + 1, Length(itemsArray));
    commaPos := Pos(',', itemsArray);
  end;
  itemsList.Add(itemsArray);
  Result := itemsList;


end;

function ExtractArrayID(line, subLine: String): TStringList;
var
  fieldValue, recSep: String;
  fieldStart, fieldEnd: Integer;
  itemsArray, field: String;
  commaPos, uuidPos: Integer;
  itemsList: TStringList;
begin
  recSep := #30 + #30;
  fieldStart := Pos(subLine, line); // search pos of subLine inside line
  fieldValue := Copy(line, fieldStart + Length(subLine), Length(line));
  fieldValue := stringreplace(fieldValue, recSep, '',[rfReplaceAll]);
  fieldValue := stringreplace(fieldValue, '"', '',[rfReplaceAll]);
  fieldEnd   := Pos(']', fieldValue);
  itemsArray := Copy(fieldValue, 1, fieldEnd - 1);
  itemsList := TStringList.Create;
  commaPos := Pos(',', itemsArray);
  while commaPos > 0 do
  begin
    field := Copy(itemsArray, 1, commaPos - 1);
    uuidPos := Pos('"@id":"', field);
    field := Copy(field, uuidPos + 7, Length(field));
    uuidPos := Pos('"', field);
    field := Copy(field, 1, uuidPos - 1);
    itemsList.Add(field);
    itemsArray := Copy(itemsArray, commaPos + 1, Length(itemsArray));
    commaPos := Pos(',', itemsArray);
  end;
  itemsList.Add(itemsArray);
  Result := itemsList;
end;

function ExtractArrayRefID(line, subLine: String): TArray<String>;
var
  fieldValue: String;
  posStart, posEnd, idx: Integer;
  listId: TArray<String>;
begin
  posStart := Pos(subLine, line); // search pos of subLine inside line
  fieldValue := Copy(line, posStart + Length(subLine), Length(line));
  posEnd := Pos(']', fieldValue);
  fieldValue := Copy(fieldValue, 1, posEnd - 1);
  if Pos(',', fieldValue) > 0 then
    //listId := SplitString(',', fieldValue)
    listId := fieldValue.Split([','])
  else
  begin
    setLength(listId, 1);
    listId[0] := fieldValue;
  end;

  for idx:=0 to Length(listId) - 1 do
  begin
    fieldValue := ExtractField(listId[idx], '"@id":"');
    listId[idx] := fieldValue;
  end;

  Result := listId;

end;

function RepeatString(const s: String; count: cardinal): String;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to count do
    Result := Result + s;
end;

function CountOccurrences(substringText, stringText: String): Integer;
begin
  if (substringText = '') OR (stringText = '') OR (Pos(substringText, stringText) = 0) then
    Result := 0
  else
    Result := (Length(stringText) - Length(StringReplace(stringText, substringText, '', [rfReplaceAll]))) div  Length(substringText);
end;

end.

