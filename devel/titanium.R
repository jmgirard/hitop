library(tidyverse)

# Randomize order until official ordering is confirmed
set.seed(2025)
items <- hitopsr_items |> slice_sample(prop = 1, replace = FALSE)

# Set titanium starting indexes
FormID <- 2977
FirstQID <- 1522

# Preallocate output vector
out <- vector(mode = "character", length = nrow(items) + 2)

# Open data definition and set header
out[[
  1
]] <- str_glue(
  '<DataFormDefinition FormatVersion="101201" xmlns="Ti.Schedule.DataFormDefinition">
  <Lookup1 xmlns="http://schemas.datacontract.org/2004/07/Ti.Schedule.DataAccess.LinqToSql" xmlns:i="http://www.w3.org/2001/XMLSchema-instance">
    <LUID>{FormID}</LUID>
    <TableCode>SURVEY_NAME</TableCode>
    <Description>HiTOP-SR</Description>
    <ViewOrder>1027</ViewOrder>
    <Active>false</Active>
    <Category1>0</Category1>
    <Amount1>0.0000</Amount1>
    <Category2>0</Category2>
    <Category3>0</Category3>
    <StaticID>0</StaticID>
    <Char1>HiTOP-SR</Char1>
    <Note1 />
    <ChildTableCode />
    <UserCreated>true</UserCreated>
    <CanChangeActive>true</CanChangeActive>
    <OrderLock>false</OrderLock>
    <SurveyLookup>true</SurveyLookup>
    <RefreshID>0</RefreshID>
    <Int1>0</Int1>
    <Int2>1</Int2>
    <Int3>0</Int3>
    <Int4>0</Int4>
    <Int7>0</Int7>
    <Int8>0</Int8>
    <OwnerResourceId i:nil="true" />
    <SuperLookupId i:nil="true" />
  </Lookup1>
  <Survey_Instrument xmlns="http://schemas.datacontract.org/2004/07/Ti.Schedule.DataAccess.LinqToSql" xmlns:i="http://www.w3.org/2001/XMLSchema-instance">
    <QuestionID>{FirstQID}</QuestionID>
    <SurveyID>{FormID}</SurveyID>
    <RecordType>H</RecordType>
    <UserDefined>true</UserDefined>
    <StaticQuestionID>0</StaticQuestionID>
    <AnswerSource2 />
    <Required>false</Required>
    <ViewOrder>1</ViewOrder>
    <WebFormSequence>0</WebFormSequence>
    <ShowInTitanium>true</ShowInTitanium>
    <ShowOnWebForm>true</ShowOnWebForm>
    <Core />
    <DisplayQuestionNumber />
    <MouseOverTip />
    <ShortQuestionText />
    <QuestionText>Please consider whether there have been significant times during the last 12 months during which the following statements applied to you. Then please select the option that best describes how well each statement described you during that period.
1. Not at all
2. A little
3. Moderately
4. A lot</QuestionText>
    <UseShortQuestionText>false</UseShortQuestionText>
    <QuestionAlignment>0</QuestionAlignment>
    <QuestionWordWrap>true</QuestionWordWrap>
    <NewSection>false</NewSection>
    <InColumn>1</InColumn>
    <TotalColumns>1</TotalColumns>
    <AnswerType />
    <AnswerRowOffset>0</AnswerRowOffset>
    <AnswerAlignment>0</AnswerAlignment>
    <AnswerInputMask />
    <AnswerLength>0</AnswerLength>
    <AnswerWidth>0</AnswerWidth>
    <AnswerMin>0</AnswerMin>
    <AnswerMax>0</AnswerMax>
    <FaintUnderLine>0</FaintUnderLine>
    <IncludeOnStatsReport>true</IncludeOnStatsReport>
    <ReportGrouping />
    <FieldName />
    <FieldType />
    <FieldLength>0</FieldLength>
    <RegularExpression />
    <Int1>0</Int1>
    <Int2>0</Int2>
    <SpecialVisibilityFlags>0</SpecialVisibilityFlags>
    <ContainingSectionColumnSpan>0</ContainingSectionColumnSpan>
    <ResponseOptionLayoutStyle>0</ResponseOptionLayoutStyle>
    <StyleOverrideRule>0</StyleOverrideRule>
    <RelativeQuestionOrHeaderFontSize>0</RelativeQuestionOrHeaderFontSize>
    <RelativeAnswerFontSize>0</RelativeAnswerFontSize>
    <MaximumNumberOfFiles>0</MaximumNumberOfFiles>
    <Int3>0</Int3>
  </Survey_Instrument>'
)

# Add all items as questions
for (i in seq_len(nrow(items))) {
  item_i <- items[i, ]
  out[[i + 1]] <- str_glue(
    '  <Survey_Instrument xmlns="http://schemas.datacontract.org/2004/07/Ti.Schedule.DataAccess.LinqToSql" xmlns:i="http://www.w3.org/2001/XMLSchema-instance">
    <QuestionID>{FirstQID+i}</QuestionID>
    <SurveyID>{FormID}</SurveyID>
    <RecordType>Q</RecordType>
    <UserDefined>true</UserDefined>
    <StaticQuestionID>0</StaticQuestionID>
    <AnswerSource2 />
    <Required>false</Required>
    <ViewOrder>{i+1}</ViewOrder>
    <WebFormSequence>0</WebFormSequence>
    <ShowInTitanium>true</ShowInTitanium>
    <ShowOnWebForm>true</ShowOnWebForm>
    <Core />
    <DisplayQuestionNumber />
    <MouseOverTip />
    <ShortQuestionText />
    <QuestionText>{item_i$Text}</QuestionText>
    <UseShortQuestionText>false</UseShortQuestionText>
    <QuestionAlignment>0</QuestionAlignment>
    <QuestionWordWrap>true</QuestionWordWrap>
    <NewSection>false</NewSection>
    <InColumn>1</InColumn>
    <TotalColumns>1</TotalColumns>
    <AnswerType>OPTIONGROUPNUMERIC1</AnswerType>
    <AnswerRowOffset>0</AnswerRowOffset>
    <AnswerAlignment>0</AnswerAlignment>
    <AnswerInputMask />
    <AnswerLength>0</AnswerLength>
    <AnswerWidth>0</AnswerWidth>
    <AnswerMin>1</AnswerMin>
    <AnswerMax>4</AnswerMax>
    <FaintUnderLine>0</FaintUnderLine>
    <IncludeOnStatsReport>true</IncludeOnStatsReport>
    <ReportGrouping />
    <FieldName />
    <FieldType />
    <FieldLength>0</FieldLength>
    <RegularExpression />
    <Int1>0</Int1>
    <Int2>0</Int2>
    <SpecialVisibilityFlags>0</SpecialVisibilityFlags>
    <ContainingSectionColumnSpan>0</ContainingSectionColumnSpan>
    <ResponseOptionLayoutStyle>0</ResponseOptionLayoutStyle>
    <StyleOverrideRule>0</StyleOverrideRule>
    <RelativeQuestionOrHeaderFontSize>0</RelativeQuestionOrHeaderFontSize>
    <RelativeAnswerFontSize>0</RelativeAnswerFontSize>
    <MaximumNumberOfFiles>0</MaximumNumberOfFiles>
    <Int3>0</Int3>
  </Survey_Instrument>'
  )
}

# Close data definition
out[[length(out)]] <- '</DataFormDefinition>'

# Collapse to a single string and write to XML file
out_str <- paste(out, collapse = "\n")
write_lines(out_str, "devel/hitopsr_titanium.xml", append = FALSE)
