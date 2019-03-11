import os
import os.path as op
import IPython
from IPython.display import IFrame, Image, display
from IPython.core.display import HTML
import jupyter_client
from backports import tempfile
from graphviz import Source

def exec_swipl(code):
    temp_path, output_path, code_path, dirpath = setup_env()
    with open(temp_path, 'w') as f:
        f.write(code)

    ''' Parser '''
    textlist = []
    graphviz = False
    with open(temp_path) as lines:
        '''
        Parse for graphviz input
        '''
        for line in lines:
            textlist.append(line)
            if 'GRAPHVIZ' in line:
                textlist.pop()
                textlist.append("?- print('GRAPHVIZ').")
                graphviz = True # output is graphviz
    with open(code_path, 'w') as rules:
        rules.write(''.join(textlist))

    ''' Get Prolog Output from code.pl and assign to output_path file '''
    os.system("swipl {0:s} > {1:s}  2>&1 ".format(code_path, output_path))

    ''' Remove Prolog welcome text '''
    base = open(output_path, 'r')
    lines = base.readlines()
    base.close()
    lines = lines[:-9]

    outputlist = []
    for line in lines:
        outputlist.append(line)
        if 'GRAPHVIZ' in line:
            outputlist = []
    with open(output_path, 'w') as rules:
        rules.write(''.join(outputlist))

    ''' Return final output '''
    final = open(output_path, 'r')
    output = final.read()

    #if graphviz == True:
        #src = Source('digraph "the holy hand grenade" { rankdir=LR; 1 -> 2 -> 3 -> lob }')
        #src.format = 'png'
        #output = src.render('image.gv')
    return output

def setup_env():
    dirpath =  tempfile.mkdtemp()

    temp_path = op.join(dirpath, 'temp.pl')
    output_path = op.join(dirpath, 'out.txt')
    code_path = op.join(dirpath, 'code.pl')
    return temp_path, output_path, code_path, dirpath

"""SWI-Prolog kernel wrapper"""
from ipykernel.kernelbase import Kernel

class SwiplKernel(Kernel):
    implementation = 'SWI-Prolog'
    implementation_version = '0.0'
    language = 'Prolog'
    language_version = '1.0'
    language_info = {'name': 'swipl',
                     'mimetype': 'text/plain'}
    banner = "SWI-Prolog Kernel"

    def do_execute(self, code, silent,
                   store_history=True,
                   user_expressions=None,
                   allow_stdin=False):
        """This function is called when a code cell is executed."""
        if not silent:
            output = exec_swipl(code)

            stream_content = {'name': 'stdout',
                              'text': output}
            self.send_response(self.iopub_socket,
                              'stream', stream_content)
        return {'status': 'ok',
                # The base class increments the execution
                # count
                'execution_count': self.execution_count,
                'payload': [],
                'user_expressions': {},
               }

if __name__ == '__main__':
    from ipykernel.kernelapp import IPKernelApp
    IPKernelApp.launch_instance(kernel_class=SwiplKernel)
