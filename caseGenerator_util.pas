unit caseGenerator_util;

interface
uses
  System.Classes, System.SysUtils, System.StrUtils;

function ExtractField(line, subLine: String): String;
function ExtractArray(line, subline: String): TStringList;
function ExtractArrayID(line, subline: String): TStringList;
function RepeatString(const s: String; count: cardinal): String;
function SearchItemList(itemList: String; list:TStringList): Integer;
function CountOccurrences(substringText, stringText: String): Integer;

implementation




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
  itemsArray: String;
  commaPos: Integer;
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
    itemsList.Add(Copy(itemsArray, 1, commaPos - 1));
    itemsArray := Copy(itemsArray, commaPos + 1, Length(itemsArray));
    commaPos := Pos(',', itemsArray);
  end;
  itemsList.Add(itemsArray);
  Result := itemsList;
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

