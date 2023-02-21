import csv
import json
import sys
import random
from collections import defaultdict

from copy import deepcopy
from lxml import etree
from lxml.html import fragment_fromstring

from settings import *

RANDOM = random.Random(RANDOM_SEED)
ALPHABET = (
    'abcdefghijklmnopqrstuvwxyz'
    'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    '0123456789'
)


def random_str(prefix, length):
    result = prefix
    for _ in range(length):
        result += ALPHABET[RANDOM.randrange(0, len(ALPHABET))]
    return result


def main():


    status = 0

    # Optional, read and write the template but with pretty print
    with open(TEMPLATE_FILE, 'rt', encoding='utf-8') as file:
        template = json.load(file)
    with open(TEMPLATE_FILE, 'wt', encoding='utf-8') as file:
        json.dump(template, file, indent=4)

    # Read the template file
    with open(TEMPLATE_FILE, 'rt', encoding='utf-8') as file:
        template_string = '\n'.join(file.readlines())

    # Replace global placeholders
    for key, value in {
        'main_label': MAIN_LABEL
    }.items():
        template_string = template_string.replace(f'%{key}%', value)

    # Parse the json
    template = json.loads(template_string)

    # Replace metadata placeholders
    template['SurveyEntry']['SurveyName'] = SURVEY_NAME
    template['SurveyEntry']['SurveyOwnerID'] = SURVEY_OWNER_ID
    template['SurveyEntry']['CreatorID'] = SURVEY_CREATOR_ID

    # Read the array file
    with open(ARRAY_FILE, 'rt', encoding='utf-8', newline='') as file:
        array = tuple(csv.reader(file))

    header = array[0]
    assert header[0] == '_'
    header = header[1:]
    array = array[1:]

    # Gather the distinct choice sets
    sets = tuple(set(x[0].split('.')[0] for x in array))
    assert all(s.startswith('set') for s in sets)
    sets = tuple(sorted(int(s[3:]) for s in sets))

    # Gather the distinct alternatives
    alts = tuple(set(x[0].split('.')[1] for x in array))
    assert all(a.startswith('alt') for a in alts)
    alts = tuple(sorted(int(a[3:]) for a in alts))

    # Organize the input array as a more convenient map
    num_values = len(header)
    array_map = dict()
    for s in sets:
        array_map[s] = dict()
        for a in alts:
            array_map[s][a] = list()
    for cell in array:
        sa = cell[0].split('.')
        s = int(sa[0][3:])
        a = int(sa[1][3:])
        array_map[s][a].extend(cell[1:])

    assert len(alts) == len(COLUMN_LABELS), \
        f"The array has {len(alts)} alternatives, please make sure" \
        " COLUMN_LABELS has the same number of elements!"

    # Keep track of block IDs and flow IDs and question IDs
    block_ids = defaultdict(list)
    flow_count = 0
    flow_ids = dict()
    qid_count = 0
    qids = defaultdict(list)

    # Copy blocks, repeat those with "%set_S%" once for each choice set

    blocks = list(filter(lambda d: d['Element'] == 'BL', template['SurveyElements']))
    assert len(blocks) == 1
    blocks = tuple(sorted(blocks[0]['Payload'].items()))
    assert tuple(int(b[0]) for b in blocks) == tuple(range(1, len(blocks) + 1))
    blocks = tuple(b[1] for b in blocks)
    new_blocks = list()
    for i, block in enumerate(blocks):
        if i == 0:
            assert block['Type'] == 'Trash'
            new_blocks.append(deepcopy(block))
        else:
            assert block['Type'] == 'Standard'
            # assert len(block['BlockElements'] == 1)

            description = block['Description']
            if '%set_S' in description:
                for s in sets:
                    new_block = deepcopy(block)
                    old_block_id = new_block['ID']
                    new_block_id = random_str('BL_', 15)
                    block_ids[old_block_id].append(new_block_id)
                    new_block['ID'] = new_block_id
                    new_block['Description'] = description.replace('%set_S%', str(s))
                    for element in new_block['BlockElements']:
                        old_qid = element['QuestionID']
                        qid_count += 1
                        new_qid = 'QID' + str(qid_count)
                        qids[old_qid].append(new_qid)
                        element['QuestionID'] = new_qid
                    new_blocks.append(new_block)

            else:
                new_block = deepcopy(block)
                for element in new_block['BlockElements']:
                    old_qid = element['QuestionID']
                    qid_count += 1
                    new_qid = 'QID' + str(qid_count)
                    qids[old_qid].append(new_qid)
                    element['QuestionID'] = new_qid
                new_blocks.append(new_block)

    for i, element in enumerate(template['SurveyElements']):
        if element['Element'] == 'BL':
            template['SurveyElements'][i]['Payload'] = {
                str(j): v for j, v in enumerate(new_blocks, 1)
            }

    # Also duplicate the block references in the survey flow(s)

    flows = list(filter(lambda d: d['Element'] == 'FL', template['SurveyElements']))
    assert len(flows) == 1
    assert set(flows[0]['Payload'].keys()) == {'Type', 'FlowID', 'Flow', 'Properties'}
    assert flows[0]['Payload']['Type'] == 'Root'
    old_flow_id = flows[0]['Payload']['FlowID']
    flow_count += 1
    new_flow_id = 'FL_' + str(flow_count)
    flow_ids[old_flow_id] = new_flow_id
    flows[0]['Payload']['FlowID'] = new_flow_id
    assert flows[0]['Payload']['Properties']['Count'] == len(flows[0]['Payload']['Flow'])
    flows = flows[0]['Payload']['Flow']
    new_flows = list()
    for flow in flows:
        assert flow['Type'] == 'Standard'
        old_block_id = flow['ID']
        if old_block_id in block_ids:
            for new_block_id in block_ids[old_block_id]:
                new_flow = deepcopy(flow)
                new_flow['ID'] = new_block_id
                flow_count += 1
                old_flow_id = flow['FlowID']
                new_flow_id = 'FL_' + str(flow_count)
                flow_ids[old_flow_id] = new_flow_id
                new_flow['FlowID'] = new_flow_id
                new_flows.append(new_flow)
        else:
            new_flow = deepcopy(flow)
            flow_count += 1
            old_flow_id = flow['FlowID']
            new_flow_id = 'FL_' + str(flow_count)
            flow_ids[old_flow_id] = new_flow_id
            flow['FlowID'] = new_flow_id
            new_flows.append(new_flow)

    for i, element in enumerate(template['SurveyElements']):
        if element['Element'] == 'FL':
            template['SurveyElements'][i]['Payload']['Flow'] = new_flows
            template['SurveyElements'][i]['Payload']['Properties']['Count'] = len(new_flows)

    # Finally, copy the questions, repeating for each choice set and adapting
    # the table and the multiple choice list based on number of rows and columns

    question_counts = list(filter(lambda d: d['Element'] == 'QC', template['SurveyElements']))
    assert len(question_counts) == 1
    question_counts = question_counts[0]
    question_count = int(question_counts['SecondaryAttribute'])

    question_index = -1
    for i, question in enumerate(template['SurveyElements']):
        description = question['SecondaryAttribute']
        if question['Element'] == 'SQ' and '%set_S%' in description:
            assert question_index == -1, "Expected only a single question with %set_S%"
            question_index = i
    question = template['SurveyElements'][i]

    new_questions = list(deepcopy(question) for _ in range(len(sets)))
    first = True
    for new_question, s in zip(new_questions, sets):
        if first:
            first = False
        else:
            question_count += 1
            new_question['PrimaryAttribute'] = f'QID{question_count}'
        new_question['SecondaryAttribute'] = new_question['SecondaryAttribute'].replace('%set_S%', str(s))
        payload = new_question['Payload']
        assert payload['QuestionType'] == 'MC', 'Only multiple choice questions supported'
        payload['DataExportTag'] = payload['DataExportTag'].replace('%set_S%', str(s))
        payload['QuestionDescription'] = payload['QuestionDescription'].replace('%set_S%', str(s))
        payload['QuestionID'] = new_question['PrimaryAttribute']
        choice = payload['Choices']['1']['Display']
        for i, label in enumerate(COLUMN_LABELS, 1):
            payload['Choices'][str(i)] = {
                'Display': choice.replace('%column_label_X%', label)
            }
        payload['ChoiceOrder'] = list(range(1, 1 + len(COLUMN_LABELS)))

        # Now monkey-patch the HTML table

        # First increment all the "colspan" attributes by the right amount
        question_text = payload['QuestionText']
        question_text = question_text.replace('%set_S%', str(s))
        root = fragment_fromstring(question_text)
        if root.tag == 'table':
            table = root
        else:
            tables = root.xpath('.//table')
            assert len(tables) == 1
            table = tables[0]
        for colspan in table.xpath('.//td[@colspan]'):
            before = int(colspan.attrib['colspan'])
            after = before + len(COLUMN_LABELS) - 1
            colspan.attrib['colspan'] = str(after)

        # Now insert new cells in the row with column labels
        cells = table.xpath('.//tr/td[last()]//*[contains(text(), \'%column_label_X%\')]')
        assert len(cells) == 1
        cell = cells[0]
        td = cell.xpath('./ancestor::td')
        assert len(td) == 1
        td = td[0]
        tr = td.xpath('./ancestor::tr')
        assert len(tr) == 1
        tr = tr[0]
        first = True
        for _ in COLUMN_LABELS:
            if first:
                first = False
            else:
                tr.append(deepcopy(td))
        cells = tr.xpath('.//td//*[contains(text(), \'%column_label_X%\')]')
        assert len(cells) == len(COLUMN_LABELS)
        for label, cell in zip(COLUMN_LABELS, cells):
            cell.text = cell.text.replace('%column_label_X%', label)

        # And then insert rows with the actual attributes and values
        cells = table.xpath('.//tr[last()]/td//*[contains(text(), \'%row_label_Y%\')]')
        assert len(cells) == 1
        cell = cells[0]
        tr = cell.xpath('./ancestor::tr')
        assert len(tr) == 1
        tr = tr[0]
        table = tr.xpath('./parent::*')
        assert len(table) == 1
        table = table[0]

        # Add columns
        td = tr.xpath('.//td[last()]//*[contains(text(), \'%row_value_X\')]/parent::td')
        assert len(td) == 1
        td = td[0]
        first = True
        for _ in COLUMN_LABELS:
            if first:
                first = False
            else:
                tr.append(deepcopy(td))

        # Add rows
        first = True
        for _ in header:
            if first:
                first = False
            else:
                table.append(deepcopy(tr))

        # Fill in attribute labels
        cells = table.xpath('.//tr/td//*[contains(text(), \'%row_label_Y%\')]')
        assert len(cells) == len(header)
        for j, (head, cell) in enumerate(zip(header, cells)):
            cell.text = cell.text.replace('%row_label_Y%', head)

            # Fill in attribute values
            tr = cell.xpath('.//ancestor::tr')
            assert len(tr) == 1
            tr = tr[0]
            cells = tr.xpath('.//td//*[contains(text(), \'%row_value_X%\')]')
            assert len(cells) == len(COLUMN_LABELS)
            for values, cell in zip(array_map[s].values(), cells):
                cell.text = cell.text.replace('%row_value_X%', values[j])

        # Replace the final html into the question text
        question_text = etree.tostring(root).decode()
        payload['QuestionText'] = question_text

    question_counts['SecondaryAttribute'] = str(question_count)
    template['SurveyElements'][question_index:question_index + 1] = new_questions

    # Write the new survey
    with open(SURVEY_NAME.replace(' ', '_') + '.qsf', 'wt', encoding='utf-8') as file:
        json.dump(template, file, indent=4)

    return status


if __name__ == '__main__':
    sys.exit(main())
