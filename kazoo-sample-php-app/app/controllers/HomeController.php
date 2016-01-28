<?php

class HomeController extends BaseController {

    protected $layout = 'layouts.master';

    public function index() {
        $this->layout->content = View::make('home.index');
    }

}