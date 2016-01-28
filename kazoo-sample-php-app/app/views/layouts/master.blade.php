<!doctype html>
<html class="no-js" lang="en">
  <head>
    <meta charset="utf-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1.0" />
    <title>Kazoo Sample Application</title>
    {{ HTML::style('css/foundation.css') }}
    {{ HTML::script('js/modernizr.js') }}
    @yield('additional_styles')
  </head>
  <body>
    @include('home.nav')
    @include('home.pagetitle')
    
    @yield('content')
    
    {{ HTML::script('js/jquery.js') }}
    {{ HTML::script('js/foundation.min.js') }}
    @yield('additional_scripts')
    <script>
      $(document).foundation();
    </script>
    @yield('page_global_javascript', '')
  </body>
</html>
