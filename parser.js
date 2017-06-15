var parserState = {
    grammar: '',
    document: '',
    shouldTrace: false
}
function parseRaw() {
  let parser
  const shouldTrace = parserState.shouldTrace
  try {
      parser = peg.generate( parserState.grammar, { cache: true, trace: shouldTrace } )
  } catch (e) {
      return {parsed: "", trace: JSON.stringify( e, null, 2 ) }
  }

  const blanker = function() { return 'No trace compiled' }
  const tracer = shouldTrace
    ? new Tracer( parserState.document, { showTrace: false, useColor: false } )
    : { getParseTreeString: blanker, getBacktraceString: blanker }

  let parsed, trace
  try {
    parsed = parser.parse( parserState.document, { tracer } )
    trace = tracer.getParseTreeString()
  } catch (e) {
    parsed = e
    trace = tracer.getBacktraceString()
  }

  return { parsed: JSON.stringify( parsed, null, 2 ), trace: JSON.stringify( trace, null, 2 ) }
}

var parseTimer = null
function parse( grammar, document, shouldTrace, sendBack ) {
    parserState.grammar = grammar;
    parserState.document = document;
    parserState.shouldTrace = shouldTrace;

    if ( parseTimer ) {
        return;
    }

    parseTimer = window.setTimeout( function() {
        parseTimer = null;
        tic = Date.now();
        var result = parseRaw();
        toc = Date.now();

        console.log( '%cParse took ' + ( toc - tic ) + 'ms', 'color: red' );

        sendBack( result );
    }, shouldTrace ? 150 : 50 );
}