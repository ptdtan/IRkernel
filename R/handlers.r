prepare_mimebundle_kernel <- function(obj, handle_display_error = log_error) {
    # we always send text/plain, even if the user removed that from the option!
    text_bundle <- prepare_mimebundle(obj, 'text/plain', error_handler = handle_display_error)
    text_repr <- text_bundle$data[['text/plain']]

    # if the text/plain repr returns nothing, we also do
    if (is.null(text_repr) || nchar(text_repr) == 0L)
        return(list(data = NULL, metadata = NULL))
    if (getOption('jupyter.rich_display')) {
		log_debug("Hello rich display:")
        mimetypes <- setdiff(getOption('jupyter.display_mimetypes'), 'text/plain')
		log_debug("mimetypes: %s", paste(mimetypes, collapse='\n'))
        bundle <- prepare_mimebundle(obj, mimetypes, error_handler = handle_display_error)
        bundle$data[['text/plain']] <- text_repr
       	bundle$data[['text/html']] <- extract_element_html(obj)
        log_debug('Hello bundle %s', paste(capture.output(str(bundle)), collapse = '\n'))
        bundle
    } else {
    	text_bundle
    }
}
