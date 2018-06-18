unit caseGenerator_util;

interface
uses
  System.Classes, System.SysUtils;

function ExtractField(line, subLine: String): String;
function ExtractArray(line, subline: String): TStringList;
function ExtractArrayID(line, subline: String): TStringList;

implementation


{ utlCase }

function ExtractField(line, subLine: String): String;
var
  fieldValue, recSep: String;
  fieldStart, fieldEnd: Integer;
begin
  recSep := #30 + #30;
  fieldStart := Pos(subLine, line); // search pos of subLine inside line
  fieldValue := Copy(line, fieldStart + Length(subLine), Length(line));
  fieldValue := stringreplace(fieldValue, recSep, '',[rfReplaceAll]);
  fieldEnd   := Pos('"', fieldValue);
  Result := Copy(fieldValue, 1, fieldEnd - 1);

end;


function ExtractArray(line, subLine: String): TStringList;
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

end.

