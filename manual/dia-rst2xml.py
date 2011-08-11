#!/usr/bin/python

"""
A minimal front end to the Docutils Publisher, producing Docutils XML
from reStructuredText sources extended with special literate code blocks.
"""

import docutils.core
import docutils.parsers.rst

try:
    import locale
    locale.setlocale(locale.LC_ALL, '')
except:
    pass

def code_block( name, arguments, options, content, lineno,
                content_offset, block_text, state, state_machine ):
    return [docutils.nodes.raw('', '\n'.join(content), format = arguments[0])]

code_block.arguments = (1,0,0)
code_block.options = {'language' : docutils.parsers.rst.directives.unchanged}
code_block.content = 1

docutils.parsers.rst.directives.register_directive( 'codeblock', code_block )

description = ('Generates Docutils-native XML from standalone '
               'reStructuredText sources, extended with special '
               'blocks for literate code.')

if __name__ == "__main__":
  docutils.core.publish_cmdline(writer_name='xml', description=description)
