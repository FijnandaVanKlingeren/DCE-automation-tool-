import csv
import json
import sys
import random
from collections import defaultdict, deque

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
    if ARRAY_FILE is None:
        array = []
        array_header = []
    else:
        with open(ARRAY_FILE, 'rt', encoding='utf-8', newline='') as file:
            array = list(csv.reader(file))
        if not array:
            array = []
            array_header = []
        else:
            array_header = array[0]
            #assert array_header[0] == '_'
            array_header = array_header[1:]
            array = array[1:]
            if not array:
                array = []
                array_header = []

    if not array:
        column_labels = []
    else:
        column_labels = COLUMN_LABELS

    # Gather the distinct choice sets
    if not array:
        sets = tuple()
    else:
        sets = tuple(set(x[0].split('.')[0] for x in array))
        assert all(s.startswith('set') for s in sets)
        sets = tuple(sorted(int(s[3:]) for s in sets))

    # Gather the distinct alternatives
    if not array:
        alts = tuple()
    else:
        alts = tuple(set(x[0].split('.')[1] for x in array))
        assert all(a.startswith('alt') for a in alts)
        alts = tuple(sorted(int(a[3:]) for a in alts))

    # Organize the input array as a more convenient map
    num_values = len(array_header)
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

    assert len(alts) == len(column_labels), \
        f"The array has {len(alts)} alternatives, please make sure" \
        " column_labels has the same number of elements!"

    # Read the extra file
    if EXTRA_FILE is None:
        extra = []
        extra_header = []
    else:
        with open(EXTRA_FILE, 'rt', encoding='utf-8', newline='') as file:
            extra = list(csv.reader(file))
        if not extra:
            extra = []
            extra_header = []
        else:
            extra_header = extra[0]
            assert extra_header[:2] == ['question', 'label']
            assert extra_header[2:] == [f'option{i}' for i in range(1, len(extra_header) - 1)]
            extra = extra[1:]
            if not extra:
                extra = []
                extra_header = []

    for e, (label, qbody, *answers) in enumerate(extra):
        answers = list(filter(bool, answers))
        if not answers:
            extra[e] = [label, qbody, None]
        else:
            extra[e] = [label, qbody, answers]

    # Keep track of block IDs and flow IDs and question IDs
    block_ids = defaultdict(deque)
    flow_count = 0
    flow_ids = dict()
    qid_count = 0
    qids = defaultdict(deque)

    question_counts = list(filter(lambda d: d['Element'] == 'QC', template['SurveyElements']))
    assert len(question_counts) == 1
    question_counts = question_counts[0]
    question_count = int(question_counts['SecondaryAttribute'])

    # Copy blocks, repeat those with "%set_S%" once for each choice set
    # and repeat those with %extra_E% once for each extra question

    blocks = list(filter(lambda d: d['Element'] == 'BL', template['SurveyElements']))
    assert len(blocks) == 1
    blocks = tuple(sorted(blocks[0]['Payload'].items()))
    assert tuple(int(b[0]) for b in blocks) == tuple(range(1, len(blocks) + 1))
    blocks = tuple(b[1] for b in blocks)
    new_blocks = list()

    pre_block_index = []
    pre_block = None
    set_block_index = []
    set_block = None
    extra_block_index = []
    extra_text_block = None
    extra_multi_block = None
    post_block_index = []
    post_block = None
    for i, block in enumerate(blocks):
        if i == 0:
            assert block['Type'] == 'Trash'
        else:
            assert block['Type'] == 'Standard'

            description = block['Description']
            if 'Pre' == description:
                pre_block_index.append(i)
                pre_block = deepcopy(block)

            elif '%set_S%' in description:
                set_block_index.append(i)
                set_block = deepcopy(block)

            elif '%extra_text_E%' in description:
                extra_block_index.append(i)
                extra_text_block = deepcopy(block)
                assert len(extra_text_block['BlockElements']) == 1, \
                    f"The template block with text extra must have one question"
                assert extra_text_block['BlockElements'][0]['Type'] == 'Question'

            elif '%extra_multi_E%' in description:
                extra_block_index.append(i)
                extra_multi_block = deepcopy(block)
                assert len(extra_multi_block['BlockElements']) == 1, \
                    f"The template block with multi extra must have one question"
                assert extra_multi_block['BlockElements'][0]['Type'] == 'Question'

            elif 'Post' == description:
                post_block_index.append(i)
                post_block = deepcopy(block)

    assert len(pre_block_index) == 1, "Expected a single block with Pre"
    assert len(set_block_index) == 1, "Expected a single block with %set_S%"
    assert len(extra_block_index) == 2, "Expected two blocks with %extra_..._E%"
    assert len(post_block_index) == 1, "Expected a single block with Post"
    extra_done = False

    sorted_block_ids = list()

    for i, block in enumerate(blocks):
        if i == 0:
            assert block['Type'] == 'Trash'
            new_block = deepcopy(block)
            new_blocks.append(new_block)
            sorted_block_ids.append(new_block['ID'])
        else:
            description = block['Description']
            if i in set_block_index:
                set_block_index.remove(i)
                for s in sets:
                    new_block = deepcopy(set_block)
                    description = new_block['Description']
                    old_block_id = new_block['ID']
                    new_block_id = random_str('BL_', 15)
                    block_ids[old_block_id].append(new_block_id)
                    new_block['ID'] = new_block_id
                    sorted_block_ids.append(new_block_id)
                    new_block['Description'] = description.replace('%set_S%', str(s))
                    for element in new_block['BlockElements']:
                        old_qid = element['QuestionID']
                        qid_count += 1
                        new_qid = 'QID' + str(qid_count)
                        qids[old_qid].append(new_qid)
                        element['QuestionID'] = new_qid
                    new_blocks.append(new_block)

            elif i in extra_block_index:
                extra_block_index.remove(i)
                if extra_done:
                    continue
                extra_done = True

                for e, (qbody, label, answers) in enumerate(extra, 1):
                    if answers is None:
                        new_block = deepcopy(extra_text_block)
                        description = new_block['Description']
                        new_block['Description'] = description.replace('%extra_text_E%', str(e))
                    else:
                        new_block = deepcopy(extra_multi_block)
                        description = new_block['Description']
                        new_block['Description'] = description.replace('%extra_multi_E%', str(e))
                    old_block_id = new_block['ID']
                    new_block_id = random_str('BL_', 15)
                    block_ids[old_block_id].append(new_block_id)
                    new_block['ID'] = new_block_id
                    sorted_block_ids.append(new_block_id)

                    for element in new_block['BlockElements']:
                        old_qid = element['QuestionID']
                        qid_count += 1
                        new_qid = 'QID' + str(qid_count)
                        qids[old_qid].append(new_qid)
                        element['QuestionID'] = new_qid
                    new_blocks.append(new_block)

            else:
                assert i in pre_block_index or i in post_block_index
                new_block = deepcopy(block)
                for element in new_block['BlockElements']:
                    old_qid = element['QuestionID']
                    qid_count += 1
                    new_qid = 'QID' + str(qid_count)
                    qids[old_qid].append(new_qid)
                    element['QuestionID'] = new_qid
                new_blocks.append(new_block)
                sorted_block_ids.append(new_block['ID'])

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
    # assert flows[0]['Payload']['Properties']['Count'] == len(flows[0]['Payload']['Flow'])
    flows = flows[0]['Payload']['Flow']
    new_flows = list()

    embedded = list()
    endsurvey = list()

    for flow in flows:
        flow_type = flow['Type']
        assert flow_type in ('Standard', 'EmbeddedData', 'EndSurvey')
        old_block_id = flow.get('ID')
        if old_block_id in block_ids:
            for new_block_id in block_ids[old_block_id]:
                new_flow = deepcopy(flow)
                new_flow['ID'] = new_block_id
                flow_count += 1
                old_flow_id = flow['FlowID']
                new_flow_id = 'FL_' + str(flow_count)
                flow_ids[old_flow_id] = new_flow_id
                new_flow['FlowID'] = new_flow_id
                if new_flow['ID'] in sorted_block_ids:
                    new_flows.append(new_flow)
        else:
            new_flow = deepcopy(flow)
            flow_count += 1
            old_flow_id = flow['FlowID']
            new_flow_id = 'FL_' + str(flow_count)
            flow_ids[old_flow_id] = new_flow_id
            flow['FlowID'] = new_flow_id
            if new_flow.get('ID') in sorted_block_ids:
                new_flows.append(new_flow)
            elif flow_type == 'EmbeddedData':
                embedded.append(new_flow)
            elif flow_type == 'EndSurvey':
                endsurvey.append(new_flow)
            else:
                assert False, "Should not get here!"

    assert len(endsurvey) == 1, "Can't have multiple end of surveys!"

    new_flows.sort(key=lambda flow: sorted_block_ids.index(flow['ID']))
    new_flows = embedded + new_flows + endsurvey
    for i, new_flow in enumerate(new_flows, 2):
        new_flow['FlowID'] = f'FL_{i}'

    for i, element in enumerate(template['SurveyElements']):
        if element['Element'] == 'FL':
            template['SurveyElements'][i]['Payload']['Flow'] = new_flows
            template['SurveyElements'][i]['Payload']['Properties']['Count'] = len(new_flows) + 1

    # Re-number all the non-template questions
    for question in template['SurveyElements']:
        description = question['SecondaryAttribute']
        if question['Element'] == 'SQ' \
            and '%set_S%' not in description \
            and '%extra_label_E%' not in description:
            qid = qids[question['PrimaryAttribute']].popleft()
            question['PrimaryAttribute'] = qid
            payload = question['Payload']
            payload['QuestionID'] = question['PrimaryAttribute']

    # Now, copy the set questions, repeating for each choice set and adapting
    # the table and the multiple choice list based on number of rows and columns

    question_index = -1
    for i, question in enumerate(template['SurveyElements']):
        description = question['SecondaryAttribute']
        if question['Element'] == 'SQ' and '%set_S%' in description:
            assert question_index == -1, "Expected only a single question with %set_S%"
            question_index = i
    question = template['SurveyElements'][question_index]

    new_questions = list(deepcopy(question) for _ in range(len(sets)))
    first = True
    for new_question, s in zip(new_questions, sets):
        if first:
            first = False
        else:
            question_count += 1
        qid = qids[new_question['PrimaryAttribute']].popleft()
        new_question['PrimaryAttribute'] = qid
        new_question['SecondaryAttribute'] = new_question['SecondaryAttribute'].replace('%set_S%', str(s))
        payload = new_question['Payload']
        assert payload['QuestionType'] == 'MC', 'Only multiple choice questions supported'
        payload['DataExportTag'] = payload['DataExportTag'].replace('%set_S%', str(s))
        payload['QuestionDescription'] = payload['QuestionDescription'].replace('%set_S%', str(s))
        payload['QuestionID'] = new_question['PrimaryAttribute']
        choice = payload['Choices']['1']['Display']
        for i, label in enumerate(column_labels, 1):
            payload['Choices'][str(i)] = {
                'Display': choice.replace('%column_label_X%', label)
            }
        payload['ChoiceOrder'] = list(range(1, 1 + len(column_labels)))

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
            after = before + len(column_labels) - 1
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
        for _ in column_labels:
            if first:
                first = False
            else:
                tr.append(deepcopy(td))
        cells = tr.xpath('.//td//*[contains(text(), \'%column_label_X%\')]')
        assert len(cells) == len(column_labels)
        for label, cell in zip(column_labels, cells):
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
        for _ in column_labels:
            if first:
                first = False
            else:
                tr.append(deepcopy(td))

        # Add rows
        first = True
        for _ in array_header:
            if first:
                first = False
            else:
                table.append(deepcopy(tr))

        # Fill in attribute labels
        cells = table.xpath('.//tr/td//*[contains(text(), \'%row_label_Y%\')]')
        assert len(cells) == len(array_header)
        for j, (head, cell) in enumerate(zip(array_header, cells)):
            cell.text = cell.text.replace('%row_label_Y%', head)

            # Fill in attribute values
            tr = cell.xpath('.//ancestor::tr')
            assert len(tr) == 1
            tr = tr[0]
            cells = tr.xpath('.//td//*[contains(text(), \'%row_value_X%\')]')
            assert len(cells) == len(column_labels)
            for values, cell in zip(array_map[s].values(), cells):
                cell.text = cell.text.replace('%row_value_X%', values[j])

        # Replace the final html into the question text
        question_text = etree.tostring(root).decode()
        payload['QuestionText'] = question_text

    template['SurveyElements'][question_index:question_index + 1] = new_questions[:]

    # Now, copy the extra questions
    question_text_index = -1
    question_multi_index = -1
    for i, question in enumerate(template['SurveyElements']):
        if question['Element'] !='SQ' or '%extra_label_E%' not in question['SecondaryAttribute']:
            continue
        qtype = question['Payload']['QuestionType']
        if qtype == 'TE':
            assert question_text_index == -1, "Expected only a single extra text question"
            question_text_index = i
        elif qtype == 'MC':
            assert question_multi_index == -1, "Expected only a single extra multi question"
            question_multi_index = i
        else:
            assert False, "Expected TE or MC type for extra question"

    new_text_questions = list()
    new_multi_questions = list()
    first = True
    second = True
    for (qbody, label, answers) in extra:
        if first:
            first = False
        elif second:
            second = False
        else:
            question_count += 1
        if answers is None:
            new_question = deepcopy(template['SurveyElements'][question_text_index])
            new_question['SecondaryAttribute'] = new_question['SecondaryAttribute'].replace('%extra_text_E%', label)
            new_text_questions.append(new_question)
        else:
            new_question = deepcopy(template['SurveyElements'][question_multi_index])
            new_question['SecondaryAttribute'] = new_question['SecondaryAttribute'].replace('%extra_multi_E%', label)
            new_multi_questions.append(new_question)

        qid = qids[new_question['PrimaryAttribute']].popleft()
        new_question['PrimaryAttribute'] = qid

        payload = new_question['Payload']
        if answers is None:
            assert payload['QuestionType'] == 'TE', 'Text entry question expected'
        else:
            assert payload['QuestionType'] == 'MC', 'Multiple choice question expected'
        payload['DataExportTag'] = payload['DataExportTag'].replace('%extra_label_E%', label)
        payload['QuestionDescription'] = payload['QuestionDescription'].replace('%extra_label_E%', label)
        payload['QuestionID'] = new_question['PrimaryAttribute']

        question_text = payload['QuestionText']
        question_text = question_text.replace('%extra_question_E%', qbody)
        payload['QuestionText'] = question_text

        if answers is not None:
            payload['QuestionText_Unsafe'] = question_text
            choice = payload['Choices']['1']['Display']
            for i, answer in enumerate(answers, 1):
                payload['Choices'][str(i)] = {
                    'Display': choice.replace('%extra_E_answer_Z%', answer)
                }
            payload['ChoiceOrder'] = list(range(1, 1 + len(answers)))

    if question_text_index < question_multi_index:
        template['SurveyElements'] = template['SurveyElements'][:question_text_index] \
                                     + new_text_questions[:] \
                                     + template['SurveyElements'][question_text_index + 1:question_multi_index] \
                                     + new_multi_questions[:] \
                                     + template['SurveyElements'][question_multi_index + 1:]
    else:
        template['SurveyElements'] = template['SurveyElements'][:question_multi_index] \
                                     + new_multi_questions[:] \
                                     + template['SurveyElements'][question_multi_index +1:question_text_index] \
                                     + new_text_questions[:] \
                                     + template['SurveyElements'][question_text_index + 1:]

    # Update the overall question count
    question_counts['SecondaryAttribute'] = str(question_count)

    # Write the new survey
    with open(SURVEY_NAME.replace(' ', '_') + '.qsf', 'wt', encoding='utf-8') as file:
        json.dump(template, file, indent=4)

    return status


if __name__ == '__main__':
    sys.exit(main())
